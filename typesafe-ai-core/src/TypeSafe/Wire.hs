{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TypeSafe.Wire
-- Description : The TypeSafe HTTP API, schema for schema
-- Copyright   : (c) 2026 byteally
-- License     : BSD-3-Clause
--
-- A one-to-one Haskell mirror of the schemas in the TypeSafe OpenAPI
-- specification (<https://api.typesafe.ai/openapi.json>), version
-- 'apiSpecVersion'.
--
-- Most programs never need this module: "TypeSafe.Question" builds these
-- values from typed questions and decodes the answers into your own types.
-- Reach for the wire types when you want to
--
-- * talk to the API through your own HTTP stack, such as servant, and only
--   borrow the JSON codecs,
-- * log, store or replay raw requests and responses, or
-- * use an API feature before this SDK has typed support for it (see
--   'QuestionOther' and 'AnswerOther').
--
-- = Naming
--
-- Every schema becomes a type of the same name. Every property becomes a
-- field named after the schema, then the property in camel case:
-- @SystemOneRequest.state@ is 'systemOneRequestState' and
-- @Usage.input_tokens@ is 'usageInputTokens'. The fixed rule keeps the
-- field names unique without any language extensions and makes it simple to
-- check the bindings against a new version of the specification.
--
-- Schemas that use @oneOf@ with a @type@ discriminator (@Question@ and
-- @Answer@) become sum types. Their @type@ property is implied by the
-- constructor, so the variant records have no field for it.
--
-- = Forward compatibility
--
-- * Response objects ignore properties this version does not know about.
-- * Questions and answers of an unknown @type@ decode to 'QuestionOther' and
--   'AnswerOther' instead of failing.
-- * 'ModelName' is open: any model id or alias the API accepts can be used.
--
-- = Example
--
-- The example request from the API reference:
--
-- >>> :{
-- let request =
--       SystemOneRequest
--         { systemOneRequestState = "Help! My payouts have been failing for 3 days."
--         , systemOneRequestModel = jevLatest
--         , systemOneRequestQuestions =
--             Map.fromList
--               [ ( "is_urgent"
--                 , QuestionNoul
--                     NoulQuestion
--                       { noulQuestionInstructions = Just "Does this convey urgency?"
--                       , noulQuestionCriteria = Nothing
--                       }
--                 )
--               ]
--         }
-- :}
--
-- >>> LBS.putStrLn (encode request)
-- {"state":"Help! My payouts have been failing for 3 days.","model":"jev-latest","questions":{"is_urgent":{"type":"noul","instructions":"Does this convey urgency?"}}}
--
-- and its response:
--
-- >>> :{
-- let body = "{\"model\":\"jev-1.13.0\",\"answers\":{\"is_urgent\":{\"type\":\"noul\",\"noul\":0.95}},\"usage\":{\"input_tokens\":296,\"output_tokens\":20}}"
-- :}
--
-- >>> fmap systemOneResponseAnswers (eitherDecode body)
-- Right (fromList [("is_urgent",AnswerNoul (NoulAnswer {noulAnswerNoul = 0.95}))])
module TypeSafe.Wire
  ( -- * Specification version
    apiSpecVersion

    -- * Identifiers
  , ModelName (..)
  , jevLatest
  , jevPreview
  , QuestionId (..)

    -- * Evaluation request (@POST \/v1\/systemone@)
  , SystemOneRequest (..)
  , Question (..)
  , questionType
  , NoulQuestion (..)
  , NoulCriteria (..)
  , ChoiceQuestion (..)
  , ScoreQuestion (..)

    -- * Evaluation response
  , SystemOneResponse (..)
  , Answer (..)
  , answerType
  , NoulAnswer (..)
  , ChoiceAnswer (..)
  , ScoreAnswer (..)
  , Usage (..)

    -- * Models (@GET \/v1\/models@)
  , ModelMetadataList (..)
  , ModelMetadata (..)
  , modelMetadataReleaseDay

    -- * Validation errors (HTTP 422)
  , HTTPValidationError (..)
  , ValidationError (..)
  , LocationSegment (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson
  ( FromJSON (..)
  , FromJSONKey
  , Object
  , ToJSON (..)
  , ToJSONKey
  , Value (..)
  , object
  , pairs
  , withObject
  , withText
  , (.:)
  , (.:?)
  , (.=)
  )
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, Series)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.Generics (Generic)
import TypeSafe.Content (Content)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Aeson (encode, eitherDecode)
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS
-- >>> import qualified Data.Map.Strict as Map

-- | The @info.version@ of the OpenAPI specification these bindings mirror.
--
-- The test suite checks this against the vendored copy of the specification,
-- so it cannot silently fall out of date.
apiSpecVersion :: Text
apiSpecVersion = "0.2.0"

------------------------------------------------------------------------------
-- Identifiers

-- | The name of a model or model alias, as accepted by the request's @model@
-- field.
--
-- Aliases such as 'jevLatest' move when a new model ships. Pin a versioned id
-- (for example @\"jev-1.13.0\"@) if you have tuned thresholds against a
-- specific version. See <https://docs.typesafe.ai/models>.
newtype ModelName = ModelName {unModelName :: Text}
  deriving newtype (Eq, Ord, Show, IsString, ToJSON, FromJSON, NFData)

-- | The most recent stable, official release of Jev. The default model.
jevLatest :: ModelName
jevLatest = "jev-latest"

-- | The most recent release of Jev, whether or not it is an official one.
jevPreview :: ModelName
jevPreview = "jev-preview"

-- | A key you choose for a question. Its answer comes back under the same key.
--
-- Question ids are not sent to the model and do not influence the answer:
-- put the complete question in the instructions.
newtype QuestionId = QuestionId {unQuestionId :: Text}
  deriving newtype (Eq, Ord, Show, IsString, ToJSON, FromJSON, ToJSONKey, FromJSONKey, NFData)

------------------------------------------------------------------------------
-- Request

-- | Content and named questions to evaluate together (schema
-- @SystemOneRequest@).
data SystemOneRequest = SystemOneRequest
  { systemOneRequestState :: !Content
  -- ^ The content all questions in this request refer to.
  , systemOneRequestModel :: !ModelName
  -- ^ Name or alias of the model to use.
  , systemOneRequestQuestions :: !(Map QuestionId Question)
  -- ^ Questions keyed by a name you choose. The API requires at least one.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON SystemOneRequest where
  toJSON r =
    object
      [ "state" .= systemOneRequestState r
      , "model" .= systemOneRequestModel r
      , "questions" .= systemOneRequestQuestions r
      ]
  toEncoding r =
    pairs
      ( "state" .= systemOneRequestState r
          <> "model" .= systemOneRequestModel r
          <> "questions" .= systemOneRequestQuestions r
      )

instance FromJSON SystemOneRequest where
  parseJSON = withObject "SystemOneRequest" $ \o ->
    SystemOneRequest
      <$> o .: "state"
      <*> o .: "model"
      <*> o .: "questions"

-- | A question about the supplied content (schema @Question@).
data Question
  = -- | A yes\/no question.
    QuestionNoul !NoulQuestion
  | -- | Pick one of several named options.
    QuestionChoice !ChoiceQuestion
  | -- | Rate the content on an ordered rubric.
    QuestionScore !ScoreQuestion
  | -- | A question type this version of the SDK does not know about: the
    -- @type@ tag and the complete JSON object. It is sent exactly as given
    -- (with @type@ set to the tag), so new question types can be used before
    -- the SDK supports them.
    QuestionOther !Text !Object
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The value of the question's @type@ discriminator.
--
-- >>> questionType (QuestionNoul (NoulQuestion Nothing Nothing))
-- "noul"
questionType :: Question -> Text
questionType = \case
  QuestionNoul _ -> "noul"
  QuestionChoice _ -> "choice"
  QuestionScore _ -> "score"
  QuestionOther t _ -> t

instance ToJSON Question where
  toJSON = \case
    QuestionNoul q -> toJSON q
    QuestionChoice q -> toJSON q
    QuestionScore q -> toJSON q
    QuestionOther t o -> Object (KeyMap.insert "type" (String t) o)
  toEncoding = \case
    QuestionNoul q -> toEncoding q
    QuestionChoice q -> toEncoding q
    QuestionScore q -> toEncoding q
    QuestionOther t o -> toEncoding (KeyMap.insert "type" (String t) o)

instance FromJSON Question where
  parseJSON = withObject "Question" $ \o -> do
    tag <- o .: "type"
    case tag of
      "noul" -> QuestionNoul <$> parseJSON (Object o)
      "choice" -> QuestionChoice <$> parseJSON (Object o)
      "score" -> QuestionScore <$> parseJSON (Object o)
      other -> pure (QuestionOther other o)

-- | A yes\/no question or statement, answered with the probability of yes
-- (schema @NoulQuestion@).
data NoulQuestion = NoulQuestion
  { noulQuestionInstructions :: !(Maybe Content)
  -- ^ The yes\/no question or statement to evaluate.
  , noulQuestionCriteria :: !(Maybe NoulCriteria)
  -- ^ What counts as a yes or a no.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON NoulQuestion where
  toJSON q =
    object $
      ("type" .= ("noul" :: Text))
        : optionalPairs
          [ ("instructions", toJSON <$> noulQuestionInstructions q)
          , ("criteria", toJSON <$> noulQuestionCriteria q)
          ]
  toEncoding q =
    pairs $
      ("type" .= ("noul" :: Text))
        <> optionalSeries "instructions" (noulQuestionInstructions q)
        <> optionalSeries "criteria" (noulQuestionCriteria q)

instance FromJSON NoulQuestion where
  parseJSON = withObject "NoulQuestion" $ \o -> do
    expectType "noul" o
    NoulQuestion
      <$> o .:? "instructions"
      <*> o .:? "criteria"

-- | What counts as a yes or a no (schema @NoulCriteria@).
data NoulCriteria = NoulCriteria
  { noulCriteriaTrue :: !(Maybe Content)
  -- ^ What a yes (a value near 1) means.
  , noulCriteriaFalse :: !(Maybe Content)
  -- ^ What a no (a value near 0) means.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON NoulCriteria where
  toJSON c =
    object $
      optionalPairs
        [ ("true", toJSON <$> noulCriteriaTrue c)
        , ("false", toJSON <$> noulCriteriaFalse c)
        ]
  toEncoding c =
    pairs $
      optionalSeries "true" (noulCriteriaTrue c)
        <> optionalSeries "false" (noulCriteriaFalse c)

instance FromJSON NoulCriteria where
  parseJSON = withObject "NoulCriteria" $ \o ->
    NoulCriteria
      <$> o .:? "true"
      <*> o .:? "false"

-- | A question that selects one option from the choices you define (schema
-- @ChoiceQuestion@).
data ChoiceQuestion = ChoiceQuestion
  { choiceQuestionInstructions :: !(Maybe Content)
  -- ^ What the model should decide.
  , choiceQuestionCriteria :: ![(Text, Maybe Content)]
  -- ^ Option names and, optionally, when each applies. An option without a
  -- description is interpreted by its name alone.
  --
  -- The options are sent in list order. Names must be unique; the API allows
  -- at most 255 options.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ChoiceQuestion where
  toJSON q =
    object $
      ("type" .= ("choice" :: Text))
        : ("criteria" .= object [Key.fromText k .= v | (k, v) <- choiceQuestionCriteria q])
        : optionalPairs [("instructions", toJSON <$> choiceQuestionInstructions q)]
  toEncoding q =
    pairs $
      ("type" .= ("choice" :: Text))
        <> optionalSeries "instructions" (choiceQuestionInstructions q)
        <> Encoding.pair "criteria" (pairs (foldMap (\(k, v) -> Key.fromText k .= v) (choiceQuestionCriteria q)))

instance FromJSON ChoiceQuestion where
  parseJSON = withObject "ChoiceQuestion" $ \o -> do
    expectType "choice" o
    criteria <- o .: "criteria" >>= withObject "criteria" (traverse parseOption . KeyMap.toList)
    ChoiceQuestion
      <$> o .:? "instructions"
      <*> pure criteria
    where
      parseOption (k, v) = (,) (Key.toText k) <$> parseJSON v

-- | A question that assigns a score using an ordered rubric (schema
-- @ScoreQuestion@).
data ScoreQuestion = ScoreQuestion
  { scoreQuestionInstructions :: !(Maybe Content)
  -- ^ What the model should rate.
  , scoreQuestionCriteria :: !(NonEmpty Content)
  -- ^ Ordered level descriptions. Each description's position is its score,
  -- starting at zero. The API accepts up to 10 levels.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ScoreQuestion where
  toJSON q =
    object $
      ("type" .= ("score" :: Text))
        : ("criteria" .= scoreQuestionCriteria q)
        : optionalPairs [("instructions", toJSON <$> scoreQuestionInstructions q)]
  toEncoding q =
    pairs $
      ("type" .= ("score" :: Text))
        <> optionalSeries "instructions" (scoreQuestionInstructions q)
        <> ("criteria" .= scoreQuestionCriteria q)

instance FromJSON ScoreQuestion where
  parseJSON = withObject "ScoreQuestion" $ \o -> do
    expectType "score" o
    ScoreQuestion
      <$> o .:? "instructions"
      <*> o .: "criteria"

------------------------------------------------------------------------------
-- Response

-- | Answers keyed by question name, with the model used and token usage
-- (schema @SystemOneResponse@).
data SystemOneResponse = SystemOneResponse
  { systemOneResponseModel :: !ModelName
  -- ^ The versioned model that answered. May differ from the alias in the
  -- request.
  , systemOneResponseAnswers :: !(Map QuestionId Answer)
  -- ^ One answer per question, under the question's id.
  , systemOneResponseUsage :: !Usage
  -- ^ Token usage for the request.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON SystemOneResponse where
  toJSON r =
    object
      [ "model" .= systemOneResponseModel r
      , "answers" .= systemOneResponseAnswers r
      , "usage" .= systemOneResponseUsage r
      ]
  toEncoding r =
    pairs
      ( "model" .= systemOneResponseModel r
          <> "answers" .= systemOneResponseAnswers r
          <> "usage" .= systemOneResponseUsage r
      )

instance FromJSON SystemOneResponse where
  parseJSON = withObject "SystemOneResponse" $ \o ->
    SystemOneResponse
      <$> o .: "model"
      <*> o .: "answers"
      <*> o .: "usage"

-- | An answer whose type matches its question (schema @Answer@).
data Answer
  = -- | The answer to a 'QuestionNoul'.
    AnswerNoul !NoulAnswer
  | -- | The answer to a 'QuestionChoice'.
    AnswerChoice !ChoiceAnswer
  | -- | The answer to a 'QuestionScore'.
    AnswerScore !ScoreAnswer
  | -- | An answer type this version of the SDK does not know about: the @type@
    -- tag and the complete JSON object.
    AnswerOther !Text !Object
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The value of the answer's @type@ discriminator.
answerType :: Answer -> Text
answerType = \case
  AnswerNoul _ -> "noul"
  AnswerChoice _ -> "choice"
  AnswerScore _ -> "score"
  AnswerOther t _ -> t

instance ToJSON Answer where
  toJSON = \case
    AnswerNoul a -> toJSON a
    AnswerChoice a -> toJSON a
    AnswerScore a -> toJSON a
    AnswerOther t o -> Object (KeyMap.insert "type" (String t) o)
  toEncoding = \case
    AnswerNoul a -> toEncoding a
    AnswerChoice a -> toEncoding a
    AnswerScore a -> toEncoding a
    AnswerOther t o -> toEncoding (KeyMap.insert "type" (String t) o)

instance FromJSON Answer where
  parseJSON = withObject "Answer" $ \o -> do
    tag <- o .: "type"
    case tag of
      "noul" -> AnswerNoul <$> parseJSON (Object o)
      "choice" -> AnswerChoice <$> parseJSON (Object o)
      "score" -> AnswerScore <$> parseJSON (Object o)
      other -> pure (AnswerOther other o)

-- | The probability of a yes answer (schema @NoulAnswer@).
newtype NoulAnswer = NoulAnswer
  { noulAnswerNoul :: Double
  -- ^ From 0 (no) to 1 (yes). Values near 0.5 indicate uncertainty.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON NoulAnswer where
  toJSON a = object ["type" .= ("noul" :: Text), "noul" .= noulAnswerNoul a]
  toEncoding a = pairs ("type" .= ("noul" :: Text) <> "noul" .= noulAnswerNoul a)

instance FromJSON NoulAnswer where
  parseJSON = withObject "NoulAnswer" $ \o -> do
    expectType "noul" o
    NoulAnswer <$> o .: "noul"

-- | The selected option, confidence and probabilities of a choice question
-- (schema @ChoiceAnswer@).
data ChoiceAnswer = ChoiceAnswer
  { choiceAnswerChoice :: !Text
  -- ^ The option with the highest probability.
  , choiceAnswerConfidence :: !Double
  -- ^ Confidence in the selection, from 0 to 1.
  , choiceAnswerProbabilities :: !(Map Text Double)
  -- ^ The probability of every option, keyed by option name. Sums to
  -- approximately 1.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ChoiceAnswer where
  toJSON a =
    object
      [ "type" .= ("choice" :: Text)
      , "choice" .= choiceAnswerChoice a
      , "confidence" .= choiceAnswerConfidence a
      , "probabilities" .= choiceAnswerProbabilities a
      ]
  toEncoding a =
    pairs
      ( "type" .= ("choice" :: Text)
          <> "choice" .= choiceAnswerChoice a
          <> "confidence" .= choiceAnswerConfidence a
          <> "probabilities" .= choiceAnswerProbabilities a
      )

instance FromJSON ChoiceAnswer where
  parseJSON = withObject "ChoiceAnswer" $ \o -> do
    expectType "choice" o
    ChoiceAnswer
      <$> o .: "choice"
      <*> o .: "confidence"
      <*> o .: "probabilities"

-- | An expected score with its rubric, confidence and level probabilities
-- (schema @ScoreAnswer@).
data ScoreAnswer = ScoreAnswer
  { scoreAnswerScore :: !Double
  -- ^ The probability-weighted average of the level indices. May fall between
  -- levels.
  , scoreAnswerConfidence :: !Double
  -- ^ Confidence in the score, from 0 to 1.
  , scoreAnswerLegend :: !(Map Int Content)
  -- ^ Each level index mapped back to its description.
  , scoreAnswerProbabilities :: !(Map Int Double)
  -- ^ The probability of each level, keyed by level index. Sums to
  -- approximately 1.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ScoreAnswer where
  toJSON a =
    object
      [ "type" .= ("score" :: Text)
      , "score" .= scoreAnswerScore a
      , "confidence" .= scoreAnswerConfidence a
      , "legend" .= scoreAnswerLegend a
      , "probabilities" .= scoreAnswerProbabilities a
      ]
  toEncoding a =
    pairs
      ( "type" .= ("score" :: Text)
          <> "score" .= scoreAnswerScore a
          <> "confidence" .= scoreAnswerConfidence a
          <> "legend" .= scoreAnswerLegend a
          <> "probabilities" .= scoreAnswerProbabilities a
      )

instance FromJSON ScoreAnswer where
  parseJSON = withObject "ScoreAnswer" $ \o -> do
    expectType "score" o
    ScoreAnswer
      <$> o .: "score"
      <*> o .: "confidence"
      <*> o .: "legend"
      <*> o .: "probabilities"

-- | Token usage for a request (schema @Usage@).
data Usage = Usage
  { usageInputTokens :: !Int
  -- ^ Billable input tokens.
  , usageOutputTokens :: !Int
  -- ^ Output tokens. Currently free of charge.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON Usage where
  toJSON u = object ["input_tokens" .= usageInputTokens u, "output_tokens" .= usageOutputTokens u]
  toEncoding u = pairs ("input_tokens" .= usageInputTokens u <> "output_tokens" .= usageOutputTokens u)

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \o ->
    Usage
      <$> o .: "input_tokens"
      <*> o .: "output_tokens"

------------------------------------------------------------------------------
-- Models

-- | The models and aliases available to the account (schema
-- @ModelMetadataList@).
newtype ModelMetadataList = ModelMetadataList
  { modelMetadataListModels :: [ModelMetadata]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ModelMetadataList where
  toJSON l = object ["models" .= modelMetadataListModels l]
  toEncoding l = pairs ("models" .= modelMetadataListModels l)

instance FromJSON ModelMetadataList where
  parseJSON = withObject "ModelMetadataList" $ \o ->
    ModelMetadataList <$> o .: "models"

-- | A model or alias available to the account (schema @ModelMetadata@).
data ModelMetadata = ModelMetadata
  { modelMetadataName :: !ModelName
  -- ^ The name to send in the request's @model@ field.
  , modelMetadataDescription :: !Text
  -- ^ What the model is for.
  , modelMetadataReleaseDate :: !Text
  -- ^ The release date, formatted as @YYYY-MM-DD@. See
  -- 'modelMetadataReleaseDay'.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ModelMetadata where
  toJSON m =
    object
      [ "name" .= modelMetadataName m
      , "description" .= modelMetadataDescription m
      , "release_date" .= modelMetadataReleaseDate m
      ]
  toEncoding m =
    pairs
      ( "name" .= modelMetadataName m
          <> "description" .= modelMetadataDescription m
          <> "release_date" .= modelMetadataReleaseDate m
      )

instance FromJSON ModelMetadata where
  parseJSON = withObject "ModelMetadata" $ \o ->
    ModelMetadata
      <$> o .: "name"
      <*> o .: "description"
      <*> o .: "release_date"

-- | The release date as a 'Day', when it is a valid @YYYY-MM-DD@ date.
--
-- The date is kept as text in 'ModelMetadata' so that an unexpected format
-- never makes listing models fail.
--
-- >>> modelMetadataReleaseDay (ModelMetadata "jev-latest" "General-purpose system one model." "2026-09-15")
-- Just 2026-09-15
modelMetadataReleaseDay :: ModelMetadata -> Maybe Day
modelMetadataReleaseDay = iso8601ParseM . Text.unpack . modelMetadataReleaseDate

------------------------------------------------------------------------------
-- Validation errors

-- | The body of an HTTP 422 response (schema @HTTPValidationError@).
newtype HTTPValidationError = HTTPValidationError
  { httpValidationErrorDetail :: Maybe [ValidationError]
  -- ^ Which request values are missing or invalid.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON HTTPValidationError where
  toJSON e = object (optionalPairs [("detail", toJSON <$> httpValidationErrorDetail e)])
  toEncoding e = pairs (optionalSeries "detail" (httpValidationErrorDetail e))

instance FromJSON HTTPValidationError where
  parseJSON = withObject "HTTPValidationError" $ \o ->
    HTTPValidationError <$> o .:? "detail"

-- | One validation failure (schema @ValidationError@).
data ValidationError = ValidationError
  { validationErrorLoc :: ![LocationSegment]
  -- ^ Where the invalid value is: the request part, then field names and
  -- array indices, for example @body → questions → urgency → criteria@.
  , validationErrorMsg :: !Text
  -- ^ A human-readable explanation.
  , validationErrorType :: !Text
  -- ^ A machine-readable error code such as @missing@.
  , validationErrorInput :: !(Maybe Value)
  -- ^ The value that failed validation.
  , validationErrorCtx :: !(Maybe Object)
  -- ^ Extra context, such as the violated limit.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON ValidationError where
  toJSON e =
    object $
      [ "loc" .= validationErrorLoc e
      , "msg" .= validationErrorMsg e
      , "type" .= validationErrorType e
      ]
        <> optionalPairs
          [ ("input", validationErrorInput e)
          , ("ctx", Object <$> validationErrorCtx e)
          ]
  toEncoding e =
    pairs $
      "loc" .= validationErrorLoc e
        <> "msg" .= validationErrorMsg e
        <> "type" .= validationErrorType e
        <> optionalSeries "input" (validationErrorInput e)
        <> optionalSeries "ctx" (validationErrorCtx e)

instance FromJSON ValidationError where
  parseJSON = withObject "ValidationError" $ \o ->
    ValidationError
      <$> o .: "loc"
      <*> o .: "msg"
      <*> o .: "type"
      <*> o .:? "input"
      <*> o .:? "ctx"

-- | One step of a 'validationErrorLoc' path.
data LocationSegment
  = -- | An object key.
    LocationField !Text
  | -- | An array index.
    LocationIndex !Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance ToJSON LocationSegment where
  toJSON = \case
    LocationField t -> String t
    LocationIndex i -> toJSON i
  toEncoding = \case
    LocationField t -> toEncoding t
    LocationIndex i -> toEncoding i

instance FromJSON LocationSegment where
  parseJSON = \case
    String t -> pure (LocationField t)
    v -> LocationIndex <$> parseJSON v

------------------------------------------------------------------------------
-- Helpers

optionalPairs :: [(Text, Maybe Value)] -> [(Key.Key, Value)]
optionalPairs kvs = [(Key.fromText k, v) | (k, Just v) <- kvs]

optionalSeries :: (ToJSON v) => Key.Key -> Maybe v -> Series
optionalSeries k = maybe mempty (k .=)

-- | Require the @type@ discriminator to be the expected tag.
expectType :: Text -> Object -> Parser ()
expectType expected o = case KeyMap.lookup "type" o of
  Nothing -> fail ("missing \"type\": expected " <> show expected)
  Just v ->
    withText
      "type"
      ( \t ->
          if t == expected
            then pure ()
            else fail ("expected \"type\" to be " <> show expected <> ", got " <> show t)
      )
      v
