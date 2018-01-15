module Main where

import Prelude

import AWS.DynamoDB (DYNAMO, DynamoClient, deleteRecord, getClient, loadRecord, saveRecord)
import Control.Monad.Aff (Aff, Fiber)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn3)
import Control.Monad.Except (runExcept)
import Data.Array (foldl, length, snoc)
import Data.Either (Either(Right, Left))
import Data.Foldable (sum)
import Data.Foreign (Foreign, ForeignError, renderForeignError)
import Data.Lens (Lens', _Just, over, set, view)
import Data.Lens.Record (prop)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable)
import Data.StrMap (StrMap, alter, empty, keys, lookup)
import Data.String as Str
import Data.String.Utils (toCharArray)
import Data.Symbol (SProxy(SProxy))
import SecretWord.Words (randomFiveLetterWord, isRealWord)
import Simple.JSON (class ReadForeign, class WriteForeign, read, write, writeJSON)
import Web.AWS.Lambda (makeHandler)
import Web.Amazon.Alexa (AlexaRequest(IntentRequest, SessionEndedRequest, LaunchRequest), AlexaResponse)
import Web.Amazon.Alexa.Lens (_body, _outputSpeech, _reprompt, _response, _sessionAttributes, _shouldEndSession)

{- A Session is a piece of state defined and managed by an Alexa skill. It is persisted by Alexa
   throughout the course of a user's continuous interaction with the skill.  We keep track of the
   state of our game inside the session. Furthermore, in cases where the user stops interacting
   with the skill, we persist the session record in DynamoDB to give the user the option of
   continuing the game later.

   The state for our game is pretty simple. There is the secret word that is chosen when the user
   first begins to interact with the skill. Furthermore, there is a list of the words the user has
   guessed. Right now this is used only to tell the user once they win how many guesses it took them.
   The final piece of state, 'status' is used to keep track of when we asked the user a yes or no
   question.
-}
type Session = Maybe
  { secretWord :: String
  , guesses :: Array String
  , status :: Status
  }

{- _guesses is a "Lens". Lenses are a cool trick that let you access and modify specific parts of
   a datatype less verbosely.
-}
_guesses :: ∀ r. Lens' {guesses :: Array String | r } (Array String)
_guesses = prop (SProxy :: SProxy "guesses")

{- status is also a lens
-}
_status :: ∀ r. Lens' {status :: Status | r } Status
_status = prop (SProxy :: SProxy "status")


{- When status is Normal it means the last thing we asked/told the user was not a yes or no
   question. When status is GivingUp that means the last thing we asked the user was "do you want
   to give up?". When status is Loading it means the last thing we asked the user was "do you want
   to pick up and continue the game from where we left off last time?"
-}
data Status = Normal | GivingUp | Loading

{- WriteForeign and ReadForeign instances for the Status type determine how the "status" datatype
   should be translated to and from javascript.
-}
instance wfStatus :: WriteForeign Status where
  writeImpl Normal = write "normal"
  writeImpl GivingUp = write "GivingUp"
  writeImpl Loading = write "Loading"

instance rfStatus :: ReadForeign Status where
  readImpl f = read f <#> readStatus where
    readStatus s
      | s == "GivingUp" = GivingUp
      | s == "Loading" = Loading
      | otherwise = Normal


{- This function defines the core of the game's logic. Given two words, how many letters do they
   have in common? (Where duplicate letters contribute to the total to the extent that both words
   contain the duplication).
-}
lettersInCommon :: String → String → Int
lettersInCommon w1 w2 =
  -- Build two histograms, h1 and h2, that represent the counts of letters in w1 and w2, respectively.
  -- then, for each letter in w1, look the letter up in both histograms take the smaller of the two
  -- counts. Sum the resulting list, and that is the reported total.
  keys h1
    # map (\c →
        min
          (fromMaybe 0 (lookup c h1))
          (fromMaybe 0 (lookup c h2))
      )
    # sum
  where
    increment :: Maybe Int → Maybe Int
    increment Nothing = Just 1
    increment (Just x) = Just (x+1)

    histogram :: String → StrMap Int
    -- There has got to be a more intelligible way to write this, lol.
    histogram w = foldl (\acc f → f acc) empty (map (alter increment) (toCharArray w))

    h1 = histogram w1
    h2 = histogram w2

{-
  An empty AlexaResponse
-}
--  TODO: It's an error for a response to lack both outputSpeech and a card. That would be great to enforce via types.
emptyResponse :: AlexaResponse Session
emptyResponse =
  { version : "1.0"
  , sessionAttributes : Nothing
  , response :
    { outputSpeech : Nothing
    , card : Nothing
    , reprompt : Nothing
    , shouldEndSession : false
    }
  }

{-
  Persists a session record as a document in the DynamoDB secretword__Sessions table, which is assumed to exist.
-}
saveSession :: ∀ e. DynamoClient → String → Session → Aff (console :: CONSOLE, dynamo :: DYNAMO | e) Unit
saveSession cli userId sess = do
  case sess of
    Nothing → pure unit
    Just rec → do
      log $ "Saving session: " <> (writeJSON
                                    { userId : userId
                                    , guesses: rec.guesses
                                    , secretWord : rec.secretWord
                                    , status : Normal
                                    }
                                  )
      saveRecord
        cli
        "secretword__Sessions"
        ( write
          { userId : userId
          , guesses: rec.guesses
          , secretWord : rec.secretWord
          , status : "normal"
          }
        )
{-
  Looks up a session by the userId field in the DynamoDB secretword__Sessions table. Will
  return Nothing if no session could be found.
-}

loadSession :: ∀ e. DynamoClient → String → Aff (console :: CONSOLE, dynamo :: DYNAMO | e) (Maybe Session)
loadSession cli id = do
  rec <- loadRecord
    cli
    "secretword__Sessions"
    ( write
      { userId : id }
    )
  log ("the session was " <> (writeJSON rec))
  case runExcept (read rec) of
    Left _ → pure Nothing
    Right (result :: {"Item" :: Session})→ pure $ Just (result."Item")

{-
  Deletes a session by userId from the DynamoDB secretword__Sessions table.
-}
eraseSession :: ∀ e. DynamoClient → String → Aff (console :: CONSOLE, dynamo :: DYNAMO | e) Unit
eraseSession cli id = do
  deleteRecord
    cli
    "secretword__Sessions"
    ( write
      { userId : id }
    )

{-
  Augments an AlexaResponse with PlainText speech corresponding to the provided string. That is,
  it returns an AlexaResponse modified such that Alexa will "say" the provided string.
-}
-- TODO: It's probably a mistake for the input to this function to be an AlexaResponse that
-- already has outputSpeech set. It would be great to be able to enforce via types that
-- outputSpeech is only set once in the course of constructing an AlexaResponse.
say :: ∀ a . String → AlexaResponse a → AlexaResponse a
say speech ar = set (_response <<< _outputSpeech) (Just { type : "PlainText", text : speech }) ar

{-
  Augments an AlexaResponse with a reprompt that includes PlainText outputSpeech corresponding
  to the provided string. Alexa will deliver the reprompt if the user says nothing for 8 seconds.
-}
-- TODO: Similarly to 'say', it would be great to enforce that reprompt only gets set once.
reprompt :: ∀ a . String → AlexaResponse a → AlexaResponse a
reprompt speech ar = set (_response <<< _reprompt) (Just { outputSpeech : { type : "PlainText", text : speech }}) ar

{- Sets the 'shouldEndSession' boolean on the AlexaResponse to false. This will cause
   Alexa, after she has delivered the response, to listen for the user's next utterance
   and deliver it to the skill. The user need not say the wake word again. Furthermore,
   on the user's next utterance, the AlexaRequest provided to the skill will contain
   whatever 'session' object is attached to this AlexaResponse.
-}
keepGoing :: ∀ a. AlexaResponse a → AlexaResponse a
keepGoing ar = set (_response <<< _shouldEndSession) false ar

{- Sets the 'shouldEndSession' boolean on the AlexaResponse to false. This will cause
   Alexa not to listen for another utterance after the response is delivered. If the
   user wishes to continue interacting with the skill, he or she will have to say
   the wake word (Alexa) and invoke the skill by name
-}
stopGoing :: ∀ a. AlexaResponse a → AlexaResponse a
stopGoing ar = set (_response <<< _shouldEndSession) true ar

{- Replaces the 'session' on the AlexaResponse with the provided value
-}
setSession :: Session → AlexaResponse Session → AlexaResponse Session
setSession sess ar = set (_sessionAttributes) sess ar

{- Modifies the 'session' on the AlexaResponse to have the 'GivingUp' status. The GivingUp
  status is a signal to the program when handling the next utterance in the session that,
  if the next utterance is determined by Alexa to be an "AMAZON.YesIntent" (i.e. Alexa thinks
  the user said something like "yes") this should be interpreted as the user saying "yes,
  I wish to give up". Similarly, "AMAZON.NoIntent" should be interpreted as the user saying
  "no, I do not wish to give up".
-}
startGivingUp :: AlexaResponse Session → AlexaResponse Session
startGivingUp = set (_sessionAttributes <<< _Just <<< _status) GivingUp

{- Modifies the 'session' on the AlexaResponse to have the 'Loading' status, which is
   a signal to this program if the next utterance is an "AMAZON.YesIntent" is should
   be interpreted as "yes, I wish to pick up from where I left off last time" and if
   the next utterance is an "AMAZON.NoIntent" it should be interpreted as
   "no, I do not wish to pick up from where I left off last time.
-}
startLoading :: AlexaResponse Session → AlexaResponse Session
startLoading = set (_sessionAttributes <<< _Just <<< _status) Loading

{- Modifies the 'session' on the AlexaResponse to have the 'Normal' status. This is
  a signal to the program in handling the next utterance in the session that we did
  not ask a yes or no question -- so if the user's next utterance is determined by Alexa
  to be an "AMAZON.YesIntent" or "AMAZON.NoIntent" -- it doesn't really make any sense.
  We should probably assume that the user is guessing trying to guess a word.
-}
backToNormal :: AlexaResponse Session → AlexaResponse Session
backToNormal = set (_sessionAttributes <<< _Just <<< _status) Normal

{- Appends a word to the 'guesses' array on the session object.
-}
addGuessToSession :: String → AlexaResponse Session → AlexaResponse Session
addGuessToSession s = over (_sessionAttributes <<< _Just <<< _guesses) ((flip snoc) s)

{- This function is the entry point into the application. Every time a user interacts with the
   skill, this function is executed, receiving the (unparsed) AWS Lambda "event" and "context"
   objects.

   (I'm lying. Strictly speaking the *actual* entry point is the 'handler' function at the bottom
   of the file, which is produced by uncurrying this function. But why speak strictly?)
-}
myHandler :: ∀ e. Foreign → Foreign → Aff (dynamo :: DYNAMO, console :: CONSOLE, random :: RANDOM | e) Foreign
myHandler event _ =
  -- First, deserialize the 'event' object passed into the AWS handler into an AlexaRequest
  map write $ case (runExcept (read event)) of
    Left errs → do
      log ("Error parsing Alexa event: " <> (renderErrs errs) <> ", event" <> (writeJSON event))
      emptyResponse
        # say ("Error parsing Alexa event")
        # stopGoing
        # pure
    Right e →
      -- Then, after deserializing the event, the resulting record will have a 'sessionAttributes'
      -- field (.session.attributes that will) be of type Foreign, and it should be deserializable
      -- into our Session type.
      case (runExcept (read (view _body e).session.attributes)) of
        Left errs →
          emptyResponse
            # say ("Error parsing session: " <> (renderErrs errs))
            # stopGoing
            # pure
        Right sess → do
          log $ writeJSON event
          -- Pass the deserialized AlexaRequest and Session into the handleEvent function, which
          -- defines the logic of what to actually do when executing our skill.
          handleEvent e sess
  where
    {- When deserialization fails, typically you receive a non-empty list of ForeignErrors.
       This is a convenient function that converts such a list into a string that we can then log.
    -}
    renderErrs :: NonEmptyList ForeignError → String
    renderErrs errs =
      map renderForeignError errs
        # foldl (\x y → x <> ", " <> y) ""

    {- The dynamoClient is used to make requests to DynamoDB. If your AWS Lambda function has
       permissions configured correctly, you should not need to provide any configuration beyond
       "region". The AWS sdk will magically just work.

       Similarly, if you have an appropriate file in $HOME/.aws/config on your local development
       machine, you will be able to run the process locally and magically be able to access DynamoDB.
    -}
    dynamoClient :: DynamoClient
    dynamoClient = getClient
      { region : "us-east-1"
      , endpoint : Nothing
      , apiVersion : Nothing
      }

    {- Annoyingly, Alexa times out after 8 seconds. Often, that's not enough time for the user to
       formulate their guess. To mitigate this problem, after every attempted guess we can set a
       reprompt to annoy the user and prompt them to interact and avoid timing out. Even if the user
       chooses not to say "I'm thinking", this effectively doubles the time they have to respond to
       16 seconds.
    -}
    defaultReprompt = reprompt "Still thinking? Just say, I'm thinking."

    {- This function handles randomly selecting the five letter word, initializing an empty session
       and producing an introductory AlexaResponse.
    -}
    -- TODO: I'm not at peace about how this is a function from Unit to Aff e (AlexaResponse Session)
    --       It would make more sense to be simply an Aff value. I used to have it that way but I
    --       changed it after I noticed that "The word is ..." was being logged on *every* invocation,
    --       and this seemed to fix the problem. Does this have something to do with strict evaluation
    --       vs lazy?
    startGame :: Unit → Aff (dynamo :: DYNAMO, console :: CONSOLE, random :: RANDOM | e) (AlexaResponse Session)
    startGame _ = do
      word <- liftEff $ randomFiveLetterWord
      log $ "The word is " <> word
      emptyResponse
        # setSession (Just { secretWord : word, guesses : [], status : Normal })
        # say "OK! I've chosen a word. Play the game by guessing five-letter words. Say \"help me\" for a full description of the rules."
        # defaultReprompt
        # keepGoing
        # pure

    {- handleEvent is the central definition of the skill's logic. As you can see from the type
       signature, it is responsible for producing an AlexaResponse based on the incoming
       AlexaRequest and current Session (and the state of dynamoDB and the random number generator)
    -}
    handleEvent :: AlexaRequest → Session → Aff (dynamo :: DYNAMO, console :: CONSOLE, random :: RANDOM | e) (AlexaResponse Session)
    handleEvent (LaunchRequest r) _ = do
      log "SessionLaunched"
      oldSess <- loadSession dynamoClient r.session.user.userId
      case oldSess of
        Nothing → startGame unit
        Just (sess :: Session) →
          case sess of
            Nothing -> startGame unit
            Just _ →
              emptyResponse
                # say "Looks like you didn't finish your previous game. Would you like to pick up where we left off last time?"
                # setSession sess
                # startLoading
                # reprompt "Looks like you didn't finish your previous game. Would you like to pick up where we left off last time?"
                # pure

    {- SessionEndedRequest is sent when the session ends for some reason -- typically the user let
       Alexa time out and chose not to utter a response in time. Basically Alexa is giving one last
       chance to persist the session in DynamoDB -- so that is what we do!

       SessionEndedRequests are unique because Alexa will not actually deliver the AlexaResponse
       you respond with. It doesn't really make sense to produce an AlexaResponse, actually.
       We provide an empty one here to satisfy the type constaints, but probably it is possible to
       refactor this/change the Alexa library so that handling a SessionEndedRequest expects
       Aff e Unit, not Aff e AlexaResponse like the others.
    -}
    handleEvent (SessionEndedRequest r) sess = do
      log "SessionEnded"
      saveSession dynamoClient r.session.user.userId sess
      emptyResponse
        # pure


    {- IntentRequest is the typical request sent by Alexa to the skill. The skill defines a schema
       of "intents" that basically are *categories* for all the utterances that the user might make
       when interacting with the skill. Whenever the user says something to our skill, Alexa does
       her best job in determining which "intent" that something corresponds to, and it's up to our
       skill to take that intent and respond accordingly.

       Intents can contain "slots" that act as "capture groups" for words spoken by the user, so to
       speak. For our game we have defined a 'GuessIntent' that contains a slot entitled 'Word'. So
       when the user says "My guess is 'roast'" we can look in the Word slot and see 'roast'.

       In this repository the schema is defined in a file called models/en-US.json
    -}
    handleEvent (IntentRequest r) sess = do
      log "IntentRequested"
      handleEventHelper
      where
        handleEventHelper
          | r.request.intent.name == "AMAZON.YesIntent"    = handleYesIntent r sess
          | r.request.intent.name == "AMAZON.NoIntent"     = handleNoIntent r sess
          | r.request.intent.name == "AMAZON.HelpIntent"   = handleHelpIntent r sess
          | r.request.intent.name == "AMAZON.StopIntent"   = handleStopIntent r sess
          | r.request.intent.name == "AMAZON.CancelIntent" = handleStopIntent r sess
          | r.request.intent.name == "GuessIntent"         = handleGuessIntent r sess
          | r.request.intent.name == "GiveUpIntent"        = handleGiveUpIntent r sess
          | r.request.intent.name == "ThinkingIntent"      = handleThinkingIntent r sess
          | otherwise             =
              emptyResponse
                # say "Unknown intent"
                # stopGoing
                # pure

    handleYesIntent r sess = do
      case sess of
        Just s → case s.status of
          GivingUp → do
            eraseSession dynamoClient r.session.user.userId
            emptyResponse
              # say ("Better luck next time! My secret word was " <> s.secretWord)
              # stopGoing
              # pure
          Loading →
            emptyResponse
              # say ("All right! I've remembered your secret word from last time. What would you like to guess?")
              # setSession sess
              # keepGoing
              # defaultReprompt
              # pure
          Normal →
             handleGuessIntent r sess
        Nothing → startGame unit

    handleNoIntent r sess =
      case sess of
        Just s → case s.status of
          GivingUp →
            emptyResponse
              # setSession sess
              # backToNormal
              # say "I knew you were too brave to give up! Ok, what's your next guess?"
              # keepGoing
              # defaultReprompt
              # pure
          Loading → do
            eraseSession dynamoClient r.session.user.userId
            startGame unit
          Normal → handleGuessIntent r sess
        Nothing → handleGuessIntent r sess

    handleHelpIntent r sess =
      emptyResponse
        # setSession sess
        # backToNormal
        # say ("Play the game by guessing five-letter words. " <>
               "Every time you guess a word, I will tell you how many letters of your guess are also contained in my secret word. " <>
               "When you guess the secret word, you win! " <>
               "Try to do it in as few guesses as possible. " <>
               "For example, you can guess a word by saying \"I guess peach\". " <>
               "You can also say, \"I give up\"."
          )
        # keepGoing
        # defaultReprompt
        # pure

    handleStopIntent r sess = do
      saveSession dynamoClient r.session.user.userId sess
      emptyResponse
        # backToNormal
        # say "Bye! Come back later and we can pick up where we left off."
        # stopGoing
        # pure

    handleThinkingIntent _ sess =
      emptyResponse
        # setSession sess
        # backToNormal
        # say "Ok, take a few seconds"
        # keepGoing
        # defaultReprompt
        # pure

    handleGiveUpIntent _ sess =
      emptyResponse
        # setSession sess
        # startGivingUp
        # say "Are you sure you want to give up"
        # defaultReprompt
        # pure

    handleGuessIntent r sess =
      case sess of
        Nothing →
          emptyResponse
            # say "You are not playing a game right now"
            # stopGoing
            # pure
        Just s →
          case runExcept (read r.request.intent.slots) of
            Left _ →
              emptyResponse
                # say "I couldn't hear your guess -- please try again"
                # setSession sess
                # backToNormal
                # defaultReprompt
                # pure

            Right slots → do
              let guess = slots
                    # \(x :: {"Word" :: { value :: String }}) → x."Word".value
                    # Str.toLower

              -- "simpleResponse" handles the common case when we say something, reprompt as
              -- normal, don't modify the session, and keep the session going.
              let simpleResponse text =
                    emptyResponse
                      # setSession sess
                      # backToNormal
                      # say text
                      # defaultReprompt
                      # keepGoing

              let l = Str.length guess
              let handleGuess
                    | l /= 5 = pure $
                        simpleResponse $
                          "You guessed the word "
                            <> guess
                            <> " which is "
                            <> (show l)
                            <> " letters long. Try again with a five letter word."

                    | not $ isRealWord guess = pure $
                        simpleResponse $
                          "You guessed the word "
                            <> guess
                            <> " which is not an English word I recognize. Try again."

                    | guess == s.secretWord = do

                        eraseSession dynamoClient r.session.user.userId
                        emptyResponse
                          # say (
                              "You got it! Congratulations. It only took you "
                                <> (show $ length s.guesses + 1)
                                <> " guesses."
                            )
                          # stopGoing
                          # pure

                    | otherwise = do
                        let n = lettersInCommon guess s.secretWord
                        let letterWord = if n == 1 then "letter" else "letters"
                        emptyResponse
                          # setSession (over (_Just <<< _guesses) ((flip snoc) guess) sess)
                          # backToNormal
                          # say (
                              "You guessed the word "
                                <> guess
                                <> ", which has "
                                <> (show n)
                                <> " "
                                <> "in common with my secret word"
                            )
                          # keepGoing
                          # defaultReprompt
                          # pure
              handleGuess

{- exports.handler is expected to exist by AWS lambda. It is the uncurried version of 'myHandler'
-}
handler :: forall eff. EffFn3 ( dynamo :: DYNAMO, console :: CONSOLE , random :: RANDOM | eff) Foreign Foreign (EffFn2 ( dynamo :: DYNAMO, console :: CONSOLE , random :: RANDOM | eff) (Nullable Error) Foreign Unit) (Fiber ( dynamo :: DYNAMO, console :: CONSOLE , random :: RANDOM | eff) Unit)
handler = makeHandler myHandler
