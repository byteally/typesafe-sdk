{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : TypeSafe.Question
-- Description : Typed questions, typed answers
-- Copyright   : (c) 2026 byteally
-- License     : BSD-3-Clause
--
-- TypeSafe has three kinds of question, and each comes back as a different
-- kind of answer:
--
-- +------------+---------------------------+-----------------------------------------+
-- | Question   | Asks                      | Answer                                  |
-- +============+===========================+=========================================+
-- | 'noul'     | Is this true?             | 'Noul': the probability of yes          |
-- +------------+---------------------------+-----------------------------------------+
-- | 'choice'   | Which of these options?   | 'Choice': the option, its distribution  |
-- |            |                           | and a confidence                        |
-- +------------+---------------------------+-----------------------------------------+
-- | 'score'    | Which level of a rubric?  | 'Score': an expected level, its         |
-- |            |                           | distribution and a confidence           |
-- +------------+---------------------------+-----------------------------------------+
--
-- A @'Question' a@ is one question whose answer decodes to an @a@. Choice
-- options and Score levels are ordinary Haskell types, so an answer can only
-- ever be one of the values you offered.
--
-- Questions are combined into a request with 'ask' and the 'Applicative'
-- instance of 'Questions'. Each question needs an id, and the ids of one
-- request must be distinct.
--
-- = Example
--
-- Declare the options and levels as enumerations:
--
-- >>> :{
-- data Department = Billing | Technical | Sales
--   deriving stock (Show, Eq, Generic)
--   deriving anyclass (ChoiceOption)
-- :}
--
-- >>> :{
-- data Frustration = Calm | Frustrated | VeryAngry
--   deriving stock (Show, Eq, Generic)
--   deriving anyclass (ScoreLevel)
-- :}
--
-- Then describe the result you want and the questions that produce it:
--
-- >>> :{
-- data Triage = Triage
--   { department :: Choice Department
--   , urgent :: Noul
--   , frustration :: Score Frustration
--   }
-- :}
--
-- >>> :{
-- triage :: Questions Triage
-- triage =
--   Triage
--     <$> ask "department" (choice "Which team should handle this?")
--     <*> ask "is_urgent" (noul "Does this convey urgency?")
--     <*> ask "frustration" (score "How frustrated is the customer?")
-- :}
--
-- The option and level names come from the constructors: @Billing@ is sent
-- as @billing@ and @VeryAngry@ as @very angry@. Override 'optionName',
-- 'optionDescription' and 'levelDescription' to say more.
--
-- @triage@ renders to the @questions@ of an API request
--
-- >>> either print (LBS.putStrLn . encode) (renderQuestions triage)
-- {"department":{"type":"choice","instructions":"Which team should handle this?","criteria":{"billing":null,"technical":null,"sales":null}},"frustration":{"type":"score","instructions":"How frustrated is the customer?","criteria":["calm","frustrated","very angry"]},"is_urgent":{"type":"noul","instructions":"Does this convey urgency?"}}
--
-- and decodes the answers of the response:
--
-- >>> :{
-- let Right answers = eitherDecode $ LBS.concat
--       [ "{\"department\":{\"type\":\"choice\",\"choice\":\"billing\",\"confidence\":0.81,"
--       , "\"probabilities\":{\"billing\":0.88,\"technical\":0.12,\"sales\":0.0}},"
--       , "\"is_urgent\":{\"type\":\"noul\",\"noul\":0.95},"
--       , "\"frustration\":{\"type\":\"score\",\"score\":1.05,\"confidence\":0.92,"
--       , "\"legend\":{\"0\":\"calm\",\"1\":\"frustrated\",\"2\":\"very angry\"},"
--       , "\"probabilities\":{\"0\":0.0,\"1\":0.95,\"2\":0.05}}}"
--       ]
-- :}
--
-- >>> let Right result = decodeAnswers triage answers
-- >>> choiceSelected (department result)
-- Billing
-- >>> noulProbability (urgent result)
-- 0.95
-- >>> mostLikelyLevel (frustration result)
-- Frustrated
--
-- In a program you rarely call 'renderQuestions' or 'decodeAnswers'
-- yourself. Pass the 'Questions' to 'TypeSafe.Call.systemOne' and send the
-- call with a client, such as @TypeSafe.Client.send@ from the @typesafe-ai@
-- package.
--
-- = Why there is no @Monad@
--
-- 'Questions' is an 'Applicative' and deliberately not a 'Monad'. Every
-- question in a request sees the same state and is answered independently, in
-- parallel. A question cannot depend on another one's answer, so the types
-- do not let you write one that does. When a follow-up question really
-- depends on an answer, send a second request.
--
-- With @ApplicativeDo@ you can still use @do@ notation, provided no question
-- uses an earlier answer:
--
-- @
-- {-# LANGUAGE ApplicativeDo #-}
--
-- triage :: 'Questions' Triage
-- triage = do
--   department <- 'ask' \"department\" ('choice' \"Which team should handle this?\")
--   urgent <- 'ask' \"is_urgent\" ('noul' \"Does this convey urgency?\")
--   frustration <- 'ask' \"frustration\" ('score' \"How frustrated is the customer?\")
--   pure Triage {..}
-- @
--
-- = Many questions at once
--
-- 'Questions' is 'Traversable'-friendly, so fan-out patterns such as
-- scoring every passage of a search result are one 'traverse':
--
-- @
-- relevance :: Text -> [Text] -> 'Questions' [(Text, 'Noul')]
-- relevance query passages =
--   for (zip [0 :: Int ..] passages) $ \\(i, passage) ->
--     (,) passage
--       \<$\> 'ask' ('QuestionId' (\"passage_\" <> Text.pack (show i)))
--             ('noul' ('TypeSafe.Content.contentObject' [\"query\" .= query, \"passage\" .= passage, \"question\" .= (\"Does \`passage\` answer \`query\`?\" :: Text)]))
-- @
module TypeSafe.Question
  ( -- * Asking questions
    Questions
  , ask
  , askMap
  , questionIds
  , renderQuestions
  , decodeAnswers

    -- * A single question
  , Question
  , questionSpec
  , decodeAnswer

    -- * Noul: yes or no
  , noul
  , noulWith
  , Noul (..)

    -- * Choice: one of several options
  , choice
  , choiceBy
  , ChoiceOption (..)
  , Choice (..)
  , choiceProbability
  , rankedChoices

    -- * Score: a level of a rubric
  , score
  , scoreBy
  , scoreRubric
  , ScoreLevel (..)
  , Score (..)
  , scoreProbability
  , mostLikelyLevel
  , nearestLevel
  , normalizedScore

    -- * Other question types
  , rawQuestion
  , otherQuestion

    -- * Probability and confidence
  , Probability (..)
  , Confidence (..)
  , HasConfidence (..)
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), camelTo2)
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import Data.Foldable (toList, traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic, Rep)
import TypeSafe.Content (Content (..))
import TypeSafe.Error
  ( AnswerError (..)
  , AnswerProblem (..)
  , QuestionProblem (..)
  , RequestError (..)
  )
import TypeSafe.Internal.Generic
  ( GConstructorName
  , GEnumerate
  , genericConstructorName
  , genericEnumerate
  )
import TypeSafe.Wire (QuestionId (..))
import qualified TypeSafe.Wire as Wire

-- $setup
-- >>> :set -XOverloadedStrings -XDeriveGeneric -XDeriveAnyClass -XDerivingStrategies
-- >>> import GHC.Generics (Generic)
-- >>> import Data.Aeson (encode, eitherDecode)
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS

------------------------------------------------------------------------------
-- Probability and confidence

-- | A probability between 0 and 1.
--
-- Numeric literals work directly, so thresholds read naturally:
-- @noulProbability answer >= 0.8@.
newtype Probability = Probability {unProbability :: Double}
  deriving newtype (Eq, Ord, Show, Num, Fractional, Floating, Real, RealFrac, NFData, ToJSON, FromJSON)

-- | How certain the model is about a Choice or a Score, between 0 and 1.
--
-- Confidence is derived from the shape of the probability distribution: a
-- peaked distribution gives high confidence. It is not the probability of the
-- selected option. Use it to decide whether to act on an answer or escalate
-- it; see <https://docs.typesafe.ai/confidence>.
newtype Confidence = Confidence {unConfidence :: Double}
  deriving newtype (Eq, Ord, Show, Num, Fractional, Floating, Real, RealFrac, NFData, ToJSON, FromJSON)

-- | Answers that report a 'Confidence'.
class HasConfidence a where
  confidence :: a -> Confidence

------------------------------------------------------------------------------
-- Answers

-- | The answer to a 'noul' question.
newtype Noul = Noul
  { noulProbability :: Probability
  -- ^ The probability that the answer is yes. Near 1 is a strong yes, near 0
  -- a strong no, and near 0.5 means the model is unsure.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The answer to a 'choice' question over options of type @a@.
data Choice a = Choice
  { choiceSelected :: !a
  -- ^ The option with the highest probability.
  , choiceProbabilities :: !(NonEmpty (a, Probability))
  -- ^ Every option offered, in the order it was offered, with its
  -- probability. The probabilities sum to approximately 1.
  , choiceConfidence :: !Confidence
  -- ^ How certain the model is.
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving anyclass (NFData)

instance HasConfidence (Choice a) where
  confidence = choiceConfidence

-- | The probability of one option.
--
-- >>> choiceProbability "technical" (Choice "billing" (("billing", 0.88) :| [("technical", 0.12)]) 0.81)
-- 0.12
choiceProbability :: (Eq a) => a -> Choice a -> Probability
choiceProbability o = fromMaybe 0 . lookup o . toList . choiceProbabilities

-- | The options from most to least likely. Options with equal probability
-- keep the order they were offered in.
--
-- >>> rankedChoices (Choice "billing" (("sales", 0.02) :| [("billing", 0.88), ("technical", 0.1)]) 0.81)
-- ("billing",0.88) :| [("technical",0.1),("sales",2.0e-2)]
rankedChoices :: Choice a -> NonEmpty (a, Probability)
rankedChoices = NonEmpty.sortWith (Down . snd) . choiceProbabilities

-- | The answer to a 'score' question over levels of type @a@.
data Score a = Score
  { scoreValue :: !Double
  -- ^ The expected level: the probability-weighted average of the level
  -- indices, counting from 0. It can fall between two levels.
  , scoreProbabilities :: !(NonEmpty (a, Probability))
  -- ^ Every level, lowest first, with its probability. The probabilities
  -- sum to approximately 1.
  , scoreConfidence :: !Confidence
  -- ^ How certain the model is.
  }
  deriving stock (Eq, Show, Generic, Functor)
  deriving anyclass (NFData)

instance HasConfidence (Score a) where
  confidence = scoreConfidence

-- | The probability of one level.
scoreProbability :: (Eq a) => a -> Score a -> Probability
scoreProbability l = fromMaybe 0 . lookup l . toList . scoreProbabilities

-- | The level with the highest probability. On a tie, the lower level wins.
--
-- >>> mostLikelyLevel (Score 1.05 (("calm", 0) :| [("frustrated", 0.95), ("very angry", 0.05)]) 0.92)
-- "frustrated"
mostLikelyLevel :: Score a -> a
mostLikelyLevel = fst . foldr1 higher . scoreProbabilities
  where
    higher x y = if snd y > snd x then y else x

-- | The level closest to 'scoreValue'.
--
-- The expected level blends the distribution, so it can differ from
-- 'mostLikelyLevel' when the probability mass is spread out.
--
-- >>> nearestLevel (Score 1.6 (("low", 0.1) :| [("medium", 0.2), ("high", 0.7)]) 0.6)
-- "high"
nearestLevel :: Score a -> a
nearestLevel s = fst (levels NonEmpty.!! index)
  where
    levels = scoreProbabilities s
    lastIndex = NonEmpty.length levels - 1
    index = max 0 (min lastIndex (round (scoreValue s)))

-- | 'scoreValue' scaled to the range 0–1, so that scores from rubrics of
-- different lengths can be weighted and combined (see
-- <https://docs.typesafe.ai/patterns/composite-scoring>).
--
-- >>> normalizedScore (Score 1.5 (("low", 0.1) :| [("medium", 0.3), ("high", 0.6)]) 0.6)
-- 0.75
normalizedScore :: Score a -> Double
normalizedScore s
  | lastIndex <= 0 = 0
  | otherwise = scoreValue s / fromIntegral lastIndex
  where
    lastIndex = NonEmpty.length (scoreProbabilities s) - 1

------------------------------------------------------------------------------
-- Questions

-- | A single question whose answer decodes to an @a@.
--
-- Build questions with 'noul', 'choice', 'score' and friends, and adapt the
-- answer with 'fmap':
--
-- @
-- isUrgent :: 'Question' Bool
-- isUrgent = (\\a -> 'noulProbability' a >= 0.8) \<$\> 'noul' \"Does this convey urgency?\"
-- @
data Question a = Question
  { questionSpec_ :: !(Either QuestionProblem Wire.Question)
  , questionDecoder :: Wire.Answer -> Either AnswerProblem a
  }
  deriving stock (Functor)

-- | The wire representation of the question, or why it is malformed.
questionSpec :: Question a -> Either QuestionProblem Wire.Question
questionSpec = questionSpec_

-- | Decode the answer to this question.
decodeAnswer :: Question a -> Wire.Answer -> Either AnswerProblem a
decodeAnswer = questionDecoder

-- | A yes\/no question, or a statement to judge as true or false.
--
-- >>> LBS.putStrLn (either (const "") encode (questionSpec (noul "Does this convey urgency?")))
-- {"type":"noul","instructions":"Does this convey urgency?"}
noul :: Content -> Question Noul
noul instructions = noulQuestion (Wire.NoulQuestion (Just instructions) Nothing)

-- | A yes\/no question with descriptions of what a yes and a no mean.
--
-- >>> LBS.putStrLn (either (const "") encode (questionSpec (noulWith "Does this convey urgency?" "Explicitly time-sensitive" "No urgency expressed")))
-- {"type":"noul","instructions":"Does this convey urgency?","criteria":{"true":"Explicitly time-sensitive","false":"No urgency expressed"}}
noulWith
  :: Content
  -- ^ The question.
  -> Content
  -- ^ What a yes (a value near 1) means.
  -> Content
  -- ^ What a no (a value near 0) means.
  -> Question Noul
noulWith instructions yes no =
  noulQuestion
    ( Wire.NoulQuestion
        (Just instructions)
        (Just (Wire.NoulCriteria (Just yes) (Just no)))
    )

noulQuestion :: Wire.NoulQuestion -> Question Noul
noulQuestion q = Question (Right (Wire.QuestionNoul q)) $ \case
  Wire.AnswerNoul a -> Right (Noul (Probability (Wire.noulAnswerNoul a)))
  other -> Left (UnexpectedAnswerType "noul" (Wire.answerType other))

-- | Types whose values are the options of a Choice.
--
-- For an enumeration, derive the instance: the options are the constructors
-- in declaration order, named in snake case (@NeedsReview@ becomes
-- @needs_review@), without descriptions.
--
-- @
-- data Department = Billing | Technical | Sales
--   deriving stock (Show, Eq, Generic)
--   deriving anyclass ('ChoiceOption')
-- @
--
-- Descriptions tell the model when each option applies. Add them by
-- overriding 'optionDescription':
--
-- @
-- instance 'ChoiceOption' Department where
--   'optionDescription' = Just . \\case
--     Billing -> \"Payments, invoicing, refunds\"
--     Technical -> \"Bugs, outages, integrations\"
--     Sales -> \"Pricing, upgrades, new accounts\"
-- @
--
-- The option name is what the model reads when there is no description,
-- so choose constructor names (or override 'optionName') with care. Names
-- must be distinct.
class ChoiceOption a where
  -- | Every option, in the order it is offered.
  choiceOptions :: NonEmpty a
  default choiceOptions :: (Generic a, GEnumerate (Rep a)) => NonEmpty a
  choiceOptions = genericEnumerate

  -- | The name sent to the API, and expected back in the answer.
  optionName :: a -> Text
  default optionName :: (Generic a, GConstructorName (Rep a)) => a -> Text
  optionName = Text.pack . camelTo2 '_' . genericConstructorName

  -- | When this option applies. 'Nothing' lets the name speak for itself.
  optionDescription :: a -> Maybe Content
  optionDescription _ = Nothing

-- | Pick one option of a 'ChoiceOption' type.
--
-- The option type is usually inferred from where the answer is used; you can
-- also fix it with a type application: @choice \@Department \"Which team?\"@.
choice :: (ChoiceOption a) => Content -> Question (Choice a)
choice instructions = choiceBy optionName optionDescription instructions choiceOptions

-- | Pick one of the given options, named and described by the two
-- functions. Use it when the options are only known at run time, such as a
-- catalogue loaded from a database.
--
-- >>> let skills = ("pdf", "Read and fill PDF files") :| [("xlsx", "Edit spreadsheets")]
-- >>> let q = choiceBy fst (Just . ContentText . snd) "Which skill fits the request?" skills
-- >>> LBS.putStrLn (either (const "") encode (questionSpec q))
-- {"type":"choice","instructions":"Which skill fits the request?","criteria":{"pdf":"Read and fill PDF files","xlsx":"Edit spreadsheets"}}
--
-- The names must be distinct. Otherwise the question is rejected, before
-- anything is sent, with 'DuplicateOption'.
choiceBy
  :: (a -> Text)
  -- ^ The name of an option.
  -> (a -> Maybe Content)
  -- ^ When the option applies, if the name is not enough.
  -> Content
  -- ^ What the model should decide.
  -> NonEmpty a
  -- ^ The options, in the order they are offered.
  -> Question (Choice a)
choiceBy name describe instructions options = Question spec decode
  where
    named = fmap (\o -> (name o, o)) options
    table = Map.fromList (toList named)
    spec = case firstDuplicate (map fst (toList named)) of
      Just dup -> Left (DuplicateOption dup)
      Nothing ->
        Right . Wire.QuestionChoice $
          Wire.ChoiceQuestion
            { Wire.choiceQuestionInstructions = Just instructions
            , Wire.choiceQuestionCriteria = [(n, describe o) | (n, o) <- toList named]
            }
    decode = \case
      Wire.AnswerChoice a -> do
        selected <- lookupOption (Wire.choiceAnswerChoice a)
        let probabilities = Wire.choiceAnswerProbabilities a
        traverse_ lookupOption (Map.keys probabilities)
        pure
          Choice
            { choiceSelected = selected
            , choiceProbabilities =
                fmap (\(n, o) -> (o, Probability (Map.findWithDefault 0 n probabilities))) named
            , choiceConfidence = Confidence (Wire.choiceAnswerConfidence a)
            }
      other -> Left (UnexpectedAnswerType "choice" (Wire.answerType other))
    lookupOption n = maybe (Left (UnknownOption n)) Right (Map.lookup n table)

-- | Types whose values are the levels of a Score rubric, lowest first.
--
-- For an enumeration, derive the instance: the levels are the constructors in
-- declaration order, described by their names in lower case words
-- (@VeryAngry@ becomes @very angry@).
--
-- Good level descriptions make scores far more consistent, so consider
-- writing them out:
--
-- @
-- data Severity = Cosmetic | Degraded | Outage
--   deriving stock (Show, Eq, Generic)
--
-- instance 'ScoreLevel' Severity where
--   'levelDescription' = \\case
--     Cosmetic -> \"Cosmetic: nothing is broken\"
--     Degraded -> \"Degraded: a feature misbehaves but there is a workaround\"
--     Outage -> \"Outage: customers cannot use the product\"
-- @
class ScoreLevel a where
  -- | Every level, lowest first.
  scoreLevels :: NonEmpty a
  default scoreLevels :: (Generic a, GEnumerate (Rep a)) => NonEmpty a
  scoreLevels = genericEnumerate

  -- | What the level means.
  levelDescription :: a -> Content
  default levelDescription :: (Generic a, GConstructorName (Rep a)) => a -> Content
  levelDescription = ContentText . Text.pack . camelTo2 ' ' . genericConstructorName

-- | Rate the state on the levels of a 'ScoreLevel' type.
score :: (ScoreLevel a) => Content -> Question (Score a)
score instructions = scoreBy levelDescription instructions scoreLevels

-- | Rate the state on the given levels, lowest first, described by the
-- function.
scoreBy
  :: (a -> Content)
  -- ^ What a level means.
  -> Content
  -- ^ What the model should rate.
  -> NonEmpty a
  -- ^ The levels, lowest first. The API accepts up to 10.
  -> Question (Score a)
scoreBy describe instructions levels = Question spec decode
  where
    levelCount = NonEmpty.length levels
    spec =
      Right . Wire.QuestionScore $
        Wire.ScoreQuestion
          { Wire.scoreQuestionInstructions = Just instructions
          , Wire.scoreQuestionCriteria = fmap describe levels
          }
    decode = \case
      Wire.AnswerScore a -> do
        let probabilities = Wire.scoreAnswerProbabilities a
        traverse_
          (\i -> unless (i >= 0 && i < levelCount) (Left (UnknownLevel i)))
          (Map.keys probabilities)
        pure
          Score
            { scoreValue = Wire.scoreAnswerScore a
            , scoreProbabilities =
                NonEmpty.zipWith
                  (\i l -> (l, Probability (Map.findWithDefault 0 i probabilities)))
                  (0 :| [1 ..])
                  levels
            , scoreConfidence = Confidence (Wire.scoreAnswerConfidence a)
            }
      other -> Left (UnexpectedAnswerType "score" (Wire.answerType other))

-- | Rate the state on a rubric given as descriptions, lowest first. The
-- levels of the answer are their indices, counting from 0.
--
-- >>> let q = scoreRubric "How frustrated is the customer?" ("Calm" :| ["Frustrated", "Very angry"])
-- >>> LBS.putStrLn (either (const "") encode (questionSpec q))
-- {"type":"score","instructions":"How frustrated is the customer?","criteria":["Calm","Frustrated","Very angry"]}
scoreRubric :: Content -> NonEmpty Content -> Question (Score Int)
scoreRubric instructions descriptions =
  fmap fst <$> scoreBy snd instructions (NonEmpty.zip (0 :| [1 ..]) descriptions)

-- | Send a question as-is and receive its answer as-is.
rawQuestion :: Wire.Question -> Question Wire.Answer
rawQuestion q = Question (Right q) Right

-- | A question of a type this SDK does not support yet, given by its @type@
-- tag and the rest of its JSON object. The answer is decoded with its
-- 'FromJSON' instance, from the complete answer object (including @type@).
--
-- This keeps new question types usable, with typed answers, before the SDK
-- adds first-class support for them.
otherQuestion :: (FromJSON a) => Text -> Object -> Question a
otherQuestion tag body = Question (Right (Wire.QuestionOther tag body)) $ \answer ->
  if Wire.answerType answer == tag
    then first (UndecodableAnswer . Text.pack) (parseEither parseJSON (toJSON answer))
    else Left (UnexpectedAnswerType tag (Wire.answerType answer))

------------------------------------------------------------------------------
-- Several questions

-- | Named questions to send in one request, and how to assemble their
-- answers into an @a@.
--
-- Combine them with 'ask' and the 'Applicative' instance; see the module
-- header for an example.
data Questions a = Questions
  { questionsEntries :: !(Seq (QuestionId, Either QuestionProblem Wire.Question))
  , questionsDecoder :: Map QuestionId Wire.Answer -> Collect a
  }
  deriving stock (Functor)

instance Applicative Questions where
  pure x = Questions Seq.empty (const (Collected x))
  Questions e1 d1 <*> Questions e2 d2 = Questions (e1 <> e2) (\answers -> d1 answers <*> d2 answers)

-- | Ask a question under an id. Its answer comes back under the same id; the
-- id itself is not shown to the model.
ask :: QuestionId -> Question a -> Questions a
ask qid q = Questions (Seq.singleton (qid, questionSpec q)) $ \answers ->
  case Map.lookup qid answers of
    Nothing -> failed (AnswerError qid MissingAnswer)
    Just a -> either (failed . AnswerError qid) Collected (decodeAnswer q a)
  where
    failed e = Failed (e :| [])

-- | Ask every question of a map, keyed by its id.
askMap :: Map QuestionId (Question a) -> Questions (Map QuestionId a)
askMap = Map.traverseWithKey ask

-- | The ids of the questions, in the order they were asked.
questionIds :: Questions a -> [QuestionId]
questionIds = map fst . toList . questionsEntries

-- | The @questions@ object of a request.
--
-- Fails if there are no questions, if two share an id, or if a question is
-- malformed; the first problem in the order of asking is reported.
--
-- >>> renderQuestions (pure ())
-- Left NoQuestions
--
-- >>> renderQuestions (ask "q" (noul "Is it?") *> ask "q" (noul "Is it really?"))
-- Left (DuplicateQuestionId "q")
renderQuestions :: Questions a -> Either RequestError (Map QuestionId Wire.Question)
renderQuestions qs
  | Seq.null entries = Left NoQuestions
  | otherwise = go Map.empty (toList entries)
  where
    entries = questionsEntries qs
    go acc [] = Right acc
    go acc ((qid, spec) : rest)
      | Map.member qid acc = Left (DuplicateQuestionId qid)
      | otherwise = case spec of
          Left problem -> Left (InvalidQuestion qid problem)
          Right q -> go (Map.insert qid q acc) rest

-- | Assemble the answers of a response. Reports every unusable answer, not
-- only the first.
decodeAnswers :: Questions a -> Map QuestionId Wire.Answer -> Either (NonEmpty AnswerError) a
decodeAnswers qs answers = case questionsDecoder qs answers of
  Failed errs -> Left errs
  Collected a -> Right a

------------------------------------------------------------------------------
-- Internals

-- | Like @Either (NonEmpty AnswerError)@, but '<*>' collects every error.
data Collect a
  = Failed !(NonEmpty AnswerError)
  | Collected a
  deriving stock (Functor)

instance Applicative Collect where
  pure = Collected
  Failed e1 <*> Failed e2 = Failed (e1 <> e2)
  Failed e <*> Collected _ = Failed e
  Collected _ <*> Failed e = Failed e
  Collected f <*> Collected x = Collected (f x)

firstDuplicate :: (Ord a) => [a] -> Maybe a
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (x : xs)
      | Set.member x seen = Just x
      | otherwise = go (Set.insert x seen) xs
