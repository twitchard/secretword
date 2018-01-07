module Main where

import Prelude

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
import Data.Foreign (Foreign)
import Data.Lens (Lens', _Just, over, set, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable)
import Data.Record (delete, get, insert)
import Data.StrMap (StrMap, alter, empty, keys, lookup)
import Data.String as Str
import Data.String.Utils (toCharArray)
import Data.Symbol (class IsSymbol, SProxy(SProxy))
import SecretWord.Words (randomFiveLetterWord, isRealWord)
import Simple.JSON (read, write)
import Type.Row (class RowLacks)
import Web.AWS.Lambda (makeHandler)
import Web.Amazon.Alexa (AlexaRequest(IntentRequest, SessionEndedRequest, LaunchRequest), AlexaResponse)
import Web.Amazon.Alexa.Lens (_body, _outputSpeech, _reprompt, _response, _sessionAttributes, _shouldEndSession)

rename :: forall prev next ty input inter output
   . IsSymbol prev
  => IsSymbol next
  => RowCons prev ty inter input
  => RowLacks prev inter
  => RowCons next ty inter output
  => RowLacks next inter
  => SProxy prev
  -> SProxy next
  -> Record input
  -> Record output
rename prev next record =
  insert next value inter
  where
    value = get prev record
    inter :: Record inter
    inter = delete prev record

type Session = Maybe { secretWord :: String, guesses :: Array String, givingUp :: Boolean }

_secretWord :: ∀ r. Lens' {secretWord :: String | r } String
_secretWord = prop (SProxy :: SProxy "secretWord")

_guesses :: ∀ r. Lens' {guesses :: Array String | r } (Array String)
_guesses = prop (SProxy :: SProxy "guesses")

_givingUp :: ∀ r. Lens' {givingUp :: Boolean | r } Boolean
_givingUp = prop (SProxy :: SProxy "givingUp")

lettersInCommon :: String → String → Int
lettersInCommon w1 w2 = 
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
    histogram w = foldl (\acc f → f acc) empty (map (alter increment) (toCharArray w))

    h1 = histogram w1
    h2 = histogram w2

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

say :: ∀ a . String → AlexaResponse a → AlexaResponse a
say speech ar = set (_response <<< _outputSpeech) (Just { type : "PlainText", text : speech }) ar

reprompt :: ∀ a . String → AlexaResponse a → AlexaResponse a
reprompt speech ar = set (_response <<< _reprompt) (Just { outputSpeech : { type : "PlainText", text : speech }}) ar

keepGoing :: ∀ a. AlexaResponse a → AlexaResponse a
keepGoing ar = set (_response <<< _shouldEndSession) false ar

stopGoing :: ∀ a. AlexaResponse a → AlexaResponse a
stopGoing ar = set (_response <<< _shouldEndSession) true ar

setSession :: Session → AlexaResponse Session → AlexaResponse Session
setSession sess ar = set (_sessionAttributes) sess ar

startGivingUp :: AlexaResponse Session → AlexaResponse Session
startGivingUp = set (_sessionAttributes <<< _Just <<< _givingUp) true

addGuessToSession :: String → AlexaResponse Session → AlexaResponse Session
addGuessToSession s = over (_sessionAttributes <<< _Just <<< _guesses) ((flip snoc) s)

myHandler :: ∀ e. Foreign → Foreign → Aff (console :: CONSOLE, random :: RANDOM | e) Foreign
myHandler event _ = 
  map write $ case (runExcept (read event)) of
    Left _ →
      emptyResponse
        # say ("Error parsing Alexa event: " <> "errString")
        # stopGoing
        # pure
    Right e → case (runExcept (read (view _body e).session.attributes)) of
      Left _ → 
        emptyResponse
          # say ("Error parsing session: " <> "errString")
          # stopGoing
          # pure
      Right sess → handleEvent e sess
  where
    defaultReprompt = reprompt "Still thinking? Just say, I'm thinking."

    startGame :: Unit → Aff (console :: CONSOLE, random :: RANDOM | e) (AlexaResponse Session)
    startGame _ = do
      word <- liftEff $ randomFiveLetterWord
      log $ "The word is " <> word
      emptyResponse
        # setSession (Just { secretWord : word, guesses : [], givingUp: false })
        # say "OK! I've chosen a word. Play the game by guessing five-letter words. I will tell you the number of letters your guess and the secret word have in common. Say \"help me\" for a full description of the rules."
        # defaultReprompt
        # keepGoing
        # pure

    handleEvent :: AlexaRequest → Session → Aff (console :: CONSOLE, random :: RANDOM | e) (AlexaResponse Session)
    handleEvent (LaunchRequest r) _ = startGame unit
    handleEvent (SessionEndedRequest r) _ =
      emptyResponse
        # pure
    handleEvent (IntentRequest r) sess
      | r.request.intent.name == "AMAZON.YesIntent" = handleYesIntent r sess
      | r.request.intent.name == "AMAZON.NoIntent" = handleNoIntent r sess
      | r.request.intent.name == "AMAZON.HelpIntent" =
          emptyResponse
            # setSession sess
            # say "Win the game by guessing my secret five-letter word. You can learn more about my secret word by guessing other five letter words. Each time you guess, I will tell you the total number of letters in the word you guess that are also in my secret word. Duplicate letters count add one to the total for each duplication in both the guess and the secret word. For example, if the secret word is sweet and you guess cheer, I will say 2 because both letters contain a duplicate e. But if you guessed reach, I will only say 1, because that guess contains only one e."
            # keepGoing
            # defaultReprompt
            # pure
      | r.request.intent.name == "AMAZON.StopIntent" = handleGiveUpIntent r sess
      | r.request.intent.name == "GuessIntent" = handleGuessIntent r sess
      | r.request.intent.name == "GiveUpIntent" = handleGiveUpIntent r sess
      | r.request.intent.name == "ThinkingIntent" = handleThinkingIntent r sess
      | otherwise =
          emptyResponse
            # say "Unknown intent"
            # defaultReprompt
            # stopGoing
            # pure

    handleYesIntent r sess = do
      case sess of
        Just s → if s.givingUp == true
           then 
             emptyResponse
               # say "Ha ha, you lose. My secret word was " <> s.secretWord
               # stopGoing
               # pure
           else
             handleGuessIntent r sess
        Nothing → startGame unit

    -- handleNoIntent :: _ → Aff (console :: CONSOLE, random :: RANDOM | e) (AlexaResponse Session)
    handleNoIntent r sess = 
      case sess of
        Just s → 
          if s.givingUp == true
            then emptyResponse
              # setSession sess
              # say "I knew you weren't a coward! Ok, what's your next guess?"
              # keepGoing
              # pure
            else handleGuessIntent r sess
        Nothing → handleGuessIntent r sess

    -- handleThinkingIntent :: _ → Aff (console :: CONSOLE, random :: RANDOM | e) (AlexaResponse Session)
    handleThinkingIntent _ sess =
      emptyResponse
        # setSession sess
        # say "Ok, take a few seconds"
        # defaultReprompt
        # keepGoing
        # pure
    
    -- handleGiveUpIntent :: _ → Aff (console :: CONSOLE, random :: RANDOM | e) (AlexaResponse Session)
    handleGiveUpIntent _ sess =
      emptyResponse
        # setSession sess
        # startGivingUp
        # say "Are you sure you want to give up"
        # defaultReprompt
        # pure

    -- handleGuessIntent :: _ → Aff (console :: CONSOLE, random :: RANDOM | e) (AlexaResponse Session)
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
                # defaultReprompt
                # pure

            Right slots → do
              let guess = slots
                    # rename (SProxy :: SProxy "Word") (SProxy :: SProxy "word")
                    # \(x :: {"word" :: { value :: String }}) → x.word.value
                    # Str.toLower

              let simpleResponse text =
                    emptyResponse
                      # setSession sess
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

                    | guess == s.secretWord = pure $
                        say (
                          "You got it! Congratulations. It only took you "
                            <> (show $ length s.guesses + 1)
                            <> " guesses."
                        ) $ stopGoing $ emptyResponse
                    | otherwise = do
                        let n = lettersInCommon guess s.secretWord
                        let letterWord = if n == 1 then "letter" else "letters"
                        emptyResponse
                          # setSession (over (_Just <<< _guesses) ((flip snoc) guess) sess)
                          # say (
                              "You guessed the word "
                                <> guess
                                <> ", which has "
                                <> (show n)
                                <> " "
                                <> "in common with my secret word"
                            )
                          # keepGoing
                          # pure
              handleGuess

                  
handler :: forall eff. EffFn3 ( console :: CONSOLE , random :: RANDOM | eff) Foreign Foreign (EffFn2 ( console :: CONSOLE , random :: RANDOM | eff) (Nullable Error) Foreign Unit) (Fiber ( console :: CONSOLE , random :: RANDOM | eff) Unit)
handler = makeHandler myHandler
