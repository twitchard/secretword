module Handler where

import Prelude

import AWS.DynamoDB (DYNAMO, DynamoClient, getClient)
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
import Data.Lens (_Just, over, set, view)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable)
import Data.StrMap (StrMap, alter, empty, keys, lookup)
import Data.String as Str
import Data.String.Utils (toCharArray)
import SecretWord.Words (randomFiveLetterWord, isRealWord)
import Session (Session, Status(..), _guesses, _status, eraseSession, loadSession, saveSession)
import Simple.JSON (read, write, writeJSON)
import Web.AWS.Lambda (makeHandler)
import Web.Amazon.Alexa.Helpers (emptyResponse, keepGoing, reprompt, say, setSession, stopGoing)
import Web.Amazon.Alexa.Lens (_body, _sessionAttributes)
import Web.Amazon.Alexa.Types (AlexaRequest(IntentRequest, SessionEndedRequest, LaunchRequest), AlexaResponse)

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



startGivingUp :: AlexaResponse Session → AlexaResponse Session
startGivingUp = set (_sessionAttributes <<< _Just <<< _status) GivingUp

startLoading :: AlexaResponse Session → AlexaResponse Session
startLoading = set (_sessionAttributes <<< _Just <<< _status) Loading

backToNormal :: AlexaResponse Session → AlexaResponse Session
backToNormal = set (_sessionAttributes <<< _Just <<< _status) Normal

addGuessToSession :: String → AlexaResponse Session → AlexaResponse Session
addGuessToSession s = over (_sessionAttributes <<< _Just <<< _guesses) ((flip snoc) s)

myHandler :: ∀ e. Foreign → Foreign → Aff (dynamo :: DYNAMO, console :: CONSOLE, random :: RANDOM | e) Foreign
myHandler event _ =
  map write $ case (runExcept (read event)) of
    Left errs → do
      log ("Error parsing Alexa event: " <> (renderErrs errs) <> ", event" <> (writeJSON event))
      emptyResponse Nothing
        # say ("Error parsing Alexa event")
        # stopGoing
        # pure
    Right e →
      case (runExcept (read (view _body e).session.attributes)) of
        Left errs →
          emptyResponse Nothing
            # say ("Error parsing session: " <> (renderErrs errs))
            # stopGoing
            # pure
        Right sess → do
          log $ writeJSON event
          handleEvent e sess
  where
    renderErrs :: NonEmptyList ForeignError → String
    renderErrs errs =
      map renderForeignError errs
        # foldl (\x y → x <> ", " <> y) ""

    dynamoClient :: DynamoClient
    dynamoClient = getClient
      { region : "us-east-1"
      , endpoint : Nothing
      , apiVersion : Nothing
      }

    defaultReprompt = reprompt "Still thinking? Just say, I'm thinking."

    startGame :: Unit → Aff (dynamo :: DYNAMO, console :: CONSOLE, random :: RANDOM | e) (AlexaResponse Session)
    startGame _ = do
      word <- liftEff $ randomFiveLetterWord
      log $ "The word is " <> word
      emptyResponse Nothing
        # setSession (Just { secretWord : word, guesses : [], status : Normal })
        # say "OK! I've chosen a word. Play the game by guessing five-letter words. Say \"help me\" for a full description of the rules."
        # defaultReprompt
        # keepGoing
        # pure

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
              emptyResponse Nothing
                # say "Looks like you didn't finish your previous game. Would you like to pick up where we left off last time?"
                # setSession sess
                # startLoading
                # reprompt "Looks like you didn't finish your previous game. Would you like to pick up where we left off last time?"
                # pure

    handleEvent (SessionEndedRequest r) sess = do
      log "SessionEnded"
      saveSession dynamoClient r.session.user.userId sess
      emptyResponse Nothing
        # pure


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
              emptyResponse Nothing
                # say "Unknown intent"
                # stopGoing
                # pure

    handleYesIntent r sess = do
      case sess of
        Just s → case s.status of
          GivingUp → do
            eraseSession dynamoClient r.session.user.userId
            emptyResponse Nothing
              # say ("Better luck next time! My secret word was " <> s.secretWord)
              # stopGoing
              # pure
          Loading →
            emptyResponse Nothing
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
            emptyResponse Nothing
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
      emptyResponse Nothing
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
      emptyResponse Nothing
        # backToNormal
        # say "Bye! Come back later and we can pick up where we left off."
        # stopGoing
        # pure

    handleThinkingIntent _ sess =
      emptyResponse Nothing
        # setSession sess
        # backToNormal
        # say "Ok, take a few seconds"
        # keepGoing
        # defaultReprompt
        # pure

    handleGiveUpIntent _ sess =
      emptyResponse Nothing
        # setSession sess
        # startGivingUp
        # say "Are you sure you want to give up"
        # defaultReprompt
        # pure

    handleGuessIntent r sess =
      case sess of
        Nothing →
          emptyResponse Nothing
            # say "You are not playing a game right now"
            # stopGoing
            # pure
        Just s →
          case runExcept (read r.request.intent.slots) of
            Left _ →
              emptyResponse Nothing
                # say "I couldn't hear your guess -- please try again"
                # setSession sess
                # backToNormal
                # defaultReprompt
                # pure

            Right slots → do
              let guess = slots
                    # \(x :: {"Word" :: { value :: String }}) → x."Word".value
                    # Str.toLower

              let simpleResponse text =
                    emptyResponse Nothing
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
                        emptyResponse Nothing
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
                        emptyResponse Nothing
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

handler :: forall eff. EffFn3 ( dynamo :: DYNAMO, console :: CONSOLE , random :: RANDOM | eff) Foreign Foreign (EffFn2 ( dynamo :: DYNAMO, console :: CONSOLE , random :: RANDOM | eff) (Nullable Error) Foreign Unit) (Fiber ( dynamo :: DYNAMO, console :: CONSOLE , random :: RANDOM | eff) Unit)
handler = makeHandler myHandler
