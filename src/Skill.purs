module Skill where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Except (runExcept)
import DB (class DB, eraseSession, loadSession, saveSession)
import Data.Array (catMaybes, cons, length)
import Data.Either (Either(..))
import Data.Foldable (foldl, sum, intercalate)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap (StrMap, alter, empty, keys, lookup)
import Data.String (length) as String
import Data.String (singleton, toLower, uncons)
import Data.String.Utils (toCharArray)
import SecretWord.Words (isRealWord, randomFiveLetterWord)
import Simple.JSON (read)
import Types (Card(..), CardType(..), Input(..), Output(..), Response, Session, SessionRec, SkillError(..), Speech(..), Status(..))
import Amazon.Alexa.Types (AlexaRequest(..), AlexaResponse, BuiltInIntent(..), readBuiltInIntent)

readIntent :: String → Foreign → Input
readIntent intent slots =
  case readBuiltInIntent intent of
    Just AmazonYesIntent → Yes
    Just AmazonNoIntent → No
    Just AmazonHelpIntent → Help
    Just AmazonStopIntent → Stop
    Just AmazonCancelIntent → Cancel
    Just _ → ErrorInput RequestParseError
    Nothing → readCustomIntent
    where
      readCustomIntent
        | intent == "GiveUpIntent" = GiveUp
        | intent == "ThinkingIntent" = Thinking
        | intent == "GuessIntent" = readGuess
        | intent == "SpellingIntent" = readSpelling
        | otherwise = ErrorInput IntentParseError
      readGuess = case runExcept (read slots) of
        Right (r :: {"Word" :: { value :: String} }) → Guess (toLower r."Word".value)
        Left _ → ErrorInput SlotParseError
      readSpelling = case runExcept (read slots) of
        Right (r :: { "FirstLetter"  :: Maybe { value :: String } 
                    , "SecondLetter" :: Maybe { value :: String } 
                    , "ThirdLetter"  :: Maybe { value :: String } 
                    , "FourthLetter" :: Maybe { value :: String } 
                    , "FifthLetter"  :: Maybe { value :: String } 
                    }
              ) → [ r."FirstLetter"
                  , r."SecondLetter"
                  , r."ThirdLetter"
                  , r."FourthLetter"
                  , r."FifthLetter"
                  ]
                  <#> map (\x → x.value)
                  <#> bindFlipped uncons
                  <#> map (\x → x.head)
                  <#> map singleton
                  # catMaybes
                  # foldl (<>) ""
                  # toLower
                  # Guess
                        
        Left _ → ErrorInput SlotParseError

renderResponse :: Response → AlexaResponse Session
renderResponse response =
  { version : "1.0"
  , sessionAttributes : session
  , response :
    { outputSpeech : speech
    , card : map (\(Card c) → c { type = show c.type }) card
    , reprompt : reprompt
    , shouldEndSession : shouldEnd
    }
  }
  where 
    session = response.session
    speech = case response.output of
                  JustCard c → Nothing
                  JustSpeech s → Just $ renderSpeech s.speech
                  SpeechAndCard s → Just $ renderSpeech s.speech
    card = case response.output of
                JustCard c → Just c
                SpeechAndCard sc → Just sc.card
                _ → Nothing
    reprompt = case response.output of
                    JustCard _ → Nothing
                    JustSpeech s → renderReprompt <$> renderSpeech <$> s.reprompt
                    SpeechAndCard s → renderReprompt <$> renderSpeech <$> s.reprompt
    shouldEnd = case response.session of
                     Nothing → true
                     Just _ → false

    renderSpeech (Text s) = { type : "PlainText", text : s }
    renderSpeech (SSML s) = { type : "SSML", text : s }
    
    renderReprompt x = { outputSpeech : x }

handle :: forall db e. (DB db (SkillEffects e)) => db → Foreign → Foreign → Aff (SkillEffects e) (AlexaResponse Session)
handle db event context = do
  map renderResponse $ case (runExcept (read event)) of
    Left _ → runSkill db "" (ErrorInput RequestParseError) Nothing

    Right (LaunchRequest r) → do
      let userId = r.session.user.userId
      runSkill db userId Launch Nothing

    Right (SessionEndedRequest r) → do
      let userId = r.session.user.userId
      let attrs = r.session.attributes
      let sess = case (runExcept (read attrs)) of
                   Left _ → Nothing
                   Right s → s
      runSkill db userId SessionEnded sess

    Right (IntentRequest r) → do
      let userId = r.session.user.userId
      let intent = r.request.intent.name
      let slots = r.request.intent.slots
      let attrs = r.session.attributes
      let sess = case (runExcept (read attrs)) of
                   Left _ → Nothing
                   Right s → s
      runSkill db userId (readIntent intent slots) sess


runSkill :: ∀ db e. DB db (SkillEffects e) => db → String → Input → Session → Aff (SkillEffects e) Response

runSkill _ _ (ErrorInput err) Nothing =
  pure
    { session : Nothing
    , output : JustSpeech
        { speech : speeches.couldntUnderstand
        , reprompt : Nothing
        }
    }

runSkill _ _ (ErrorInput err) sess =
  pure
    { session : sess
    , output : JustSpeech
        { speech : speeches.weirdGuess
        , reprompt : Just speeches.stillThinking
        }
    }

runSkill db userId _ Nothing = begin db userId

runSkill db userId Launch _ = begin db userId

runSkill _ _ No (Just sess@{status : Loading}) = newGame

runSkill _ _ Yes (Just sess@{status : Loading}) =
  pure
    { session : Just (sess { status = Normal } )
    , output : JustSpeech
        { speech : speeches.restoredGame
        , reprompt : Just speeches.promptToRestore
        }
    }

runSkill _ _ _ (Just sess@{status : Loading}) =
  pure
    { session : Just sess 
    , output : JustSpeech
        { speech : speeches.didntUnderstandRestore
        , reprompt : Just speeches.promptToRestore
        }
    }

runSkill _ _ Yes (Just sess@{status : GivingUp}) =
  pure
    { session : Nothing
    , output : JustSpeech
        { speech : speeches.youLose sess.secretWord
        , reprompt : Nothing
        }
    }

runSkill _ _ No (Just sess@{status : GivingUp}) =
  pure
    { session : Just sess
    , output : JustSpeech
        { speech : speeches.didntGiveUp
        , reprompt: Just speeches.stillThinking
        }
    }

runSkill _ _ _ (Just sess@{status : GivingUp}) =
  pure
    { session : Just (sess { status = Normal } )
    , output : JustSpeech
        { speech : speeches.couldntUnderstand
        , reprompt : Just speeches.stillThinking
        }
    }

runSkill _ _ Yes (Just sess) = weirdGuess sess
runSkill _ _ No (Just sess) = weirdGuess sess

runSkill _ _ Help (Just sess) = 
  pure
    { session : Just sess
    , output : JustSpeech
        { speech : speeches.instructions
        , reprompt : Just speeches.stillThinking
        }
    }

runSkill db userId (Guess guess) (Just sess) = do
  let l = String.length guess
  let handleGuess
        | l /= 5 = pure
            { session : Just sess
            , output : SpeechAndCard
                { speech : speeches.wrongLengthGuess guess l
                , reprompt : Just speeches.stillThinking
                , card : guessCard sess.secretWord sess.guesses
                }
            }
        | not $ isRealWord guess = pure
            { session : Just sess
            , output : SpeechAndCard
                { speech : speeches.unknownWord guess
                , reprompt : Just speeches.stillThinking
                , card : guessCard sess.secretWord sess.guesses
                }
            }
        | guess == sess.secretWord = do
            eraseSession db userId
            pure
              { session : Nothing
              , output : SpeechAndCard
                  { speech : speeches.youWin (length sess.guesses + 1)
                  , reprompt : Nothing
                  , card : guessCard sess.secretWord sess.guesses
                  }
              }
        | otherwise = do
            let n = lettersInCommon guess sess.secretWord
            let guesses = guess `cons` sess.guesses
            pure
              { session : Just (sess {guesses = guesses})
              , output : SpeechAndCard
                  { speech : speeches.wrongGuess guess n
                  , reprompt : Just speeches.stillThinking
                  , card : guessCard sess.secretWord guesses
                  }
              }
  handleGuess

runSkill _ _ Thinking (Just sess) = 
  pure
    { session : Just sess
    , output : SpeechAndCard
        { speech : speeches.thinking
        , reprompt : Just speeches.stillThinking
        , card : guessCard sess.secretWord sess.guesses
        }
    }

runSkill _ _ GiveUp (Just sess) = 
  pure
    { session : Just (sess {status = GivingUp})
    , output : SpeechAndCard
        { speech : speeches.giveUp
        , reprompt : Just speeches.giveUp
        , card : guessCard sess.secretWord sess.guesses
        }
    }

runSkill db userId Stop (Just sess) = goodbye db userId sess
runSkill db userId Cancel (Just sess) = goodbye db userId sess
runSkill db userId SessionEnded (Just sess) = goodbye db userId sess

type SkillEffects e = (random :: RANDOM, console :: CONSOLE | e)

begin :: ∀ db e. DB db (SkillEffects e) => db → String → Aff (SkillEffects e) Response
begin db userId = do
  loadedSession <- loadSession db userId
  case loadedSession of
    Nothing → newGame
    Just Nothing → newGame
    Just (Just sess) → pure
      { session : Just (sess { status = Loading })
      , output : JustSpeech
          { speech : speeches.promptToRestore
          , reprompt : Just speeches.promptToRestore
          }
      }

newGame :: ∀ e. Aff (random :: RANDOM | e) Response
newGame = do
  word <- liftEff $ randomFiveLetterWord
  pure 
    { session : Just
        { secretWord : word
        , guesses : []
        , status : Normal
        }
    , output : JustSpeech
        { speech : speeches.gameStarted
        , reprompt : Just $ speeches.stillThinking
        }
    }

weirdGuess :: ∀ e. SessionRec → Aff e Response
weirdGuess sess =
  pure
    { session : Just (sess { status = Normal } )
    , output : SpeechAndCard
        { speech : speeches.weirdGuess
        , reprompt : Just speeches.stillThinking
        , card : guessCard sess.secretWord sess.guesses
        }
    }

goodbye :: ∀ db e. DB db (SkillEffects e) => db → String → SessionRec → Aff (SkillEffects e) Response
goodbye db userId sess = do
  saveSession db userId (Just sess)
  pure
    { session : Nothing
    , output : JustSpeech
        { speech : speeches.goodbye
        , reprompt : Nothing
        }
    }

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

guessCard :: String → Array String → Card
guessCard secretWord guesses = Card
  { type : Simple
  , title : "Secret Word - " <> show n <> " Guesses"
  , content : guesses
      # map (\guess → guess <> " - " <> (show $ lettersInCommon guess secretWord))
      # intercalate "\n" 
  }
  where n = length guesses

speeches ::
  { gameStarted :: Speech
  , stillThinking :: Speech
  , promptToRestore :: Speech
  , didntUnderstandRestore :: Speech
  , restoredGame :: Speech
  , giveUp :: Speech
  , didntGiveUp :: Speech
  , youLose :: String -> Speech
  , youWin :: Int -> Speech
  , instructions :: Speech
  , goodbye :: Speech
  , thinking :: Speech
  , weirdGuess :: Speech
  , couldntUnderstand :: Speech
  , unknownWord :: String -> Speech
  , wrongGuess :: String → Int → Speech
  , wrongLengthGuess :: String -> Int -> Speech
  }
speeches =
  { gameStarted : Text "OK! I've chosen a word. Play the game by guessing five-letter words. Say \"help me\" for a full description of the rules."
  , stillThinking : Text "Still thinking? Just say, I'm thinking."
  , promptToRestore : Text "Looks like you didn't finish your previous game. Would you like to pick up where we left off last time?"
  , didntUnderstandRestore : Text "Sorry, please say yes or no. Do you want to restore the previous game?"
  , restoredGame : Text "All right! I've remembered your secret word from last time. What would you like to guess?"
  , giveUp : Text "Are you sure you want to give up?"
  , didntGiveUp : Text "I knew you were too brave to give up! Ok, what's your next guess?"
  , youLose : \word → Text $ "Better luck next time! My secret word was " <> word
  , youWin : \n → Text $ "You got it! Congratulations. it only took you " <> (show n) <> " guesses."
  , instructions : Text $
      "Play the game by guessing five-letter words. " <>
      "Every time you guess a word, I will tell you how many letters of your guess are also contained in my secret word. " <>
      "When you guess the secret word, you win! " <>
      "Try to do it in as few guesses as possible. " <>
      "For example, you can guess a word by saying \"I guess peach\". " <>
      "You can also say, \"I give up\"."
  , goodbye : Text "Bye! Come back later and we can pick up where we left off."
  , thinking : Text "Ok, take a few seconds"
  , couldntUnderstand : Text "I'm sorry, I didn't understand. Please try again."
  , weirdGuess : Text "I couldn't hear your guess -- please try again."
  , wrongGuess : \guess n →
      let letterWord = if n == 1 then "letter" else "letters"
      in Text $ "You guessed the word "
                <> guess
                <> ", which has "
                <> (show n)
                <> " "
                <> letterWord
                <> " in common with my secret word"
  , unknownWord : \word → Text $ "You guessed the word " <> word <> " which is not an English word I recognize. Try again."
  , wrongLengthGuess : \word n → Text $ "You guessed the word " <> word <> " which is " <> (show n) <> " letters long. Try again with a five letter word."
  }
