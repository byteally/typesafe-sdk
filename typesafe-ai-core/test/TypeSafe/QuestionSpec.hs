{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The typed question layer and calls built from it.
module TypeSafe.QuestionSpec (tests) where

import Data.Aeson (FromJSON (..), Value (..), eitherDecode, encode, object, withObject, (.:), (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types (status200, status422)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import TypeSafe.Core
import qualified TypeSafe.Wire as Wire

data Department = Billing | Technical | Sales
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ChoiceOption)

data Tone = Zealous | Angry | NeedsReview
  deriving stock (Show, Eq, Generic)

instance ChoiceOption Tone where
  optionDescription = \case
    Zealous -> Just "Enthusiastic"
    _ -> Nothing

data Frustration = Calm | Frustrated | VeryAngry
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ScoreLevel)

data Triage = Triage
  { triageDepartment :: Choice Department
  , triageUrgent :: Noul
  , triageFrustration :: Score Frustration
  }
  deriving stock (Show, Eq)

triage :: Questions Triage
triage =
  Triage
    <$> ask "department" (choice "Which team should handle this?")
    <*> ask "is_urgent" (noul "Does this convey urgency?")
    <*> ask "frustration" (score "How frustrated is the customer?")

answersOf :: LBS.ByteString -> Map QuestionId Wire.Answer
answersOf body = either error id (eitherDecode body)

sampleAnswers :: Map QuestionId Wire.Answer
sampleAnswers =
  answersOf
    "{\"department\":{\"type\":\"choice\",\"choice\":\"billing\",\"confidence\":0.81,\
    \\"probabilities\":{\"billing\":0.88,\"technical\":0.12,\"sales\":0.0}},\
    \\"is_urgent\":{\"type\":\"noul\",\"noul\":0.95},\
    \\"frustration\":{\"type\":\"score\",\"score\":1.05,\"confidence\":0.92,\
    \\"legend\":{\"0\":\"calm\",\"1\":\"frustrated\",\"2\":\"very angry\"},\
    \\"probabilities\":{\"0\":0.0,\"1\":0.95,\"2\":0.05}}}"

specOf :: Question a -> Value
specOf q = either (error . show) (either error id . eitherDecode . encode) (questionSpec q)

-- | The answers must fail to decode with exactly these problems.
decodesTo :: (Show a) => Either (NonEmpty AnswerError) a -> [AnswerError] -> Assertion
decodesTo result expected = case result of
  Left errs -> NonEmpty.toList errs @?= expected
  Right a -> assertFailure ("decoded unexpectedly: " <> show a)

tests :: TestTree
tests =
  testGroup
    "Questions"
    [ testGroup
        "rendering"
        [ testCase "derived options are snake case, in declaration order, with descriptions" $
            specOf (choice @Tone "What is the tone?")
              @?= object
                [ "type" .= ("choice" :: Text)
                , "instructions" .= ("What is the tone?" :: Text)
                , "criteria" .= object ["zealous" .= ("Enthusiastic" :: Text), "angry" .= Null, "needs_review" .= Null]
                ]
        , testCase "options are sent in declaration order, not sorted" $
            either show (LBS.unpack . encode) (questionSpec (choice @Tone "?"))
              @?= "{\"type\":\"choice\",\"instructions\":\"?\",\"criteria\":{\"zealous\":\"Enthusiastic\",\"angry\":null,\"needs_review\":null}}"
        , testCase "derived levels are lower-case words, lowest first" $
            specOf (score @Frustration "How frustrated?")
              @?= object
                [ "type" .= ("score" :: Text)
                , "instructions" .= ("How frustrated?" :: Text)
                , "criteria" .= ["calm" :: Text, "frustrated", "very angry"]
                ]
        , testCase "noulWith sends both criteria" $
            specOf (noulWith "Spam?" "Unsolicited advertising" "A legitimate message")
              @?= object
                [ "type" .= ("noul" :: Text)
                , "instructions" .= ("Spam?" :: Text)
                , "criteria" .= object ["true" .= ("Unsolicited advertising" :: Text), "false" .= ("A legitimate message" :: Text)]
                ]
        , testCase "a request needs a question" $
            renderQuestions (pure ()) @?= Left NoQuestions
        , testCase "question ids must be distinct" $
            renderQuestions (ask "a" (noul "1") *> ask "b" (noul "2") *> ask "a" (noul "3"))
              @?= Left (DuplicateQuestionId "a")
        , testCase "option names must be distinct" $
            renderQuestions (ask "tone" (choiceBy (const "same") (const Nothing) "?" (1 :| [2 :: Int])))
              @?= Left (InvalidQuestion "tone" (DuplicateOption "same"))
        , testCase "questionIds keeps the order of asking" $
            questionIds triage @?= ["department", "is_urgent", "frustration"]
        , testCase "askMap asks every question" $
            fmap Map.keys (renderQuestions (askMap (Map.fromList [("x", noul "?"), ("y", noul "!")])))
              @?= Right ["x", "y"]
        ]
    , testGroup
        "decoding"
        [ testCase "a complete response decodes to the typed result" $
            decodeAnswers triage sampleAnswers
              @?= Right
                Triage
                  { triageDepartment =
                      Choice Billing ((Billing, 0.88) :| [(Technical, 0.12), (Sales, 0)]) 0.81
                  , triageUrgent = Noul 0.95
                  , triageFrustration =
                      Score 1.05 ((Calm, 0) :| [(Frustrated, 0.95), (VeryAngry, 0.05)]) 0.92
                  }
        , testCase "every unusable answer is reported" $
            decodeAnswers triage (Map.delete "is_urgent" (Map.insert "frustration" (Wire.AnswerNoul (Wire.NoulAnswer 1)) sampleAnswers))
              `decodesTo` [ AnswerError "is_urgent" MissingAnswer
                          , AnswerError "frustration" (UnexpectedAnswerType "score" "noul")
                          ]
        , testCase "a choice outside the offered options is rejected" $
            decodeAnswers
              (ask "d" (choice @Department "?"))
              (answersOf "{\"d\":{\"type\":\"choice\",\"choice\":\"legal\",\"confidence\":1,\"probabilities\":{\"legal\":1}}}")
              `decodesTo` [AnswerError "d" (UnknownOption "legal")]
        , testCase "a probability for an option that was not offered is rejected" $
            decodeAnswers
              (ask "d" (choice @Department "?"))
              (answersOf "{\"d\":{\"type\":\"choice\",\"choice\":\"sales\",\"confidence\":1,\"probabilities\":{\"sales\":0.5,\"legal\":0.5}}}")
              `decodesTo` [AnswerError "d" (UnknownOption "legal")]
        , testCase "options missing from the probabilities count as 0" $
            fmap choiceProbabilities
              ( decodeAnswers
                  (ask "d" (choice @Department "?"))
                  (answersOf "{\"d\":{\"type\":\"choice\",\"choice\":\"sales\",\"confidence\":1,\"probabilities\":{\"sales\":1}}}")
              )
              @?= Right ((Billing, 0) :| [(Technical, 0), (Sales, 1)])
        , testCase "a score level outside the rubric is rejected" $
            decodeAnswers
              (ask "f" (score @Frustration "?"))
              (answersOf "{\"f\":{\"type\":\"score\",\"score\":3,\"confidence\":1,\"legend\":{},\"probabilities\":{\"3\":1}}}")
              `decodesTo` [AnswerError "f" (UnknownLevel 3)]
        , testCase "scoreRubric levels are indices" $
            fmap (fmap fst . scoreProbabilities)
              ( decodeAnswers
                  (ask "f" (scoreRubric "?" ("low" :| ["high"])))
                  (answersOf "{\"f\":{\"type\":\"score\",\"score\":0.2,\"confidence\":0.6,\"legend\":{},\"probabilities\":{\"0\":0.8,\"1\":0.2}}}")
              )
              @?= Right (0 :| [1])
        , testCase "otherQuestion decodes new answer types with FromJSON" $
            decodeAnswers
              (ask "r" (otherQuestion "rank" (KeyMap.fromList [("items", toJSONList' ["a", "b"])])))
              (answersOf "{\"r\":{\"type\":\"rank\",\"order\":[\"b\",\"a\"]}}")
              @?= Right (Ranking ["b", "a"])
        , testCase "rawQuestion passes the answer through" $
            decodeAnswers (ask "q" (rawQuestion (Wire.QuestionNoul (Wire.NoulQuestion (Just "?") Nothing)))) sampleAnswers
              `decodesTo` [AnswerError "q" MissingAnswer]
        ]
    , testGroup
        "answer helpers"
        [ testCase "rankedChoices sorts by probability, keeping offer order on ties" $
            rankedChoices (Choice 'b' (('a', 0.25) :| [('b', 0.5), ('c', 0.25)]) 0.5)
              @?= (('b', 0.5) :| [('a', 0.25), ('c', 0.25)])
        , testCase "choiceProbability of an unknown option is 0" $
            choiceProbability 'z' (Choice 'a' (('a', 1) :| []) 1) @?= 0
        , testCase "mostLikelyLevel prefers the lower level on a tie" $
            mostLikelyLevel (Score 0.5 ((0 :: Int, 0.5) :| [(1, 0.5)]) 0) @?= 0
        , testCase "nearestLevel clamps to the rubric" $
            map (\v -> nearestLevel (Score v ((0 :: Int, 1) :| [(1, 0), (2, 0)]) 1)) [-1, 0.4, 0.6, 1.5, 7]
              @?= [0, 0, 1, 2, 2]
        , testCase "normalizedScore scales to 0-1" $
            map (\v -> normalizedScore (Score v (('a', 1) :| [('b', 0), ('c', 0)]) 1)) [0, 1, 2] @?= [0, 0.5, 1]
        , testCase "normalizedScore of a one-level rubric is 0" $
            normalizedScore (Score 0 (('a', 1) :| []) 1) @?= 0
        , testCase "confidence is shared by Choice and Score" $
            (confidence (Choice 'a' (('a', 1) :| []) 0.7), confidence (Score 0 (('a', 1) :| []) 0.3))
              @?= (0.7, 0.3)
        ]
    , testGroup
        "calls"
        [ testCase "systemOne uses the default model" $
            bodyOf (renderCall defaults (systemOne "hi" (ask "q" (noul "?"))))
              `rightIs` Just "{\"state\":\"hi\",\"model\":\"jev-latest\",\"questions\":{\"q\":{\"type\":\"noul\",\"instructions\":\"?\"}}}"
        , testCase "withModel overrides the default model" $
            fmap (lookupKey "model") (bodyJson (renderCall defaults (withModel "jev-1.13.0" (systemOne "hi" (ask "q" (noul "?"))))))
              @?= Right (Just (String "jev-1.13.0"))
        , testCase "withExtraBody adds fields but cannot replace the SDK's" $
            fmap (\o -> (lookupKey "beam_width" o, lookupKey "model" o))
              ( bodyJson
                  ( renderCall defaults . withExtraBody (KeyMap.fromList [("beam_width", Number 4), ("model", "evil")]) $
                      systemOne "hi" (ask "q" (noul "?"))
                  )
              )
              @?= Right (Just (Number 4), Just (String "jev-latest"))
        , testCase "systemOneRaw keeps the request's model unless overridden" $ do
            let raw = Wire.SystemOneRequest "hi" "jev-1.13.0" (Map.fromList [("q", Wire.QuestionNoul (Wire.NoulQuestion (Just "?") Nothing))])
            fmap (lookupKey "model") (bodyJson (renderCall defaults (systemOneRaw raw))) @?= Right (Just (String "jev-1.13.0"))
            fmap (lookupKey "model") (bodyJson (renderCall defaults (withModel "jev-preview" (systemOneRaw raw)))) @?= Right (Just (String "jev-preview"))
        , testCase "invalid questions are rejected before sending" $
            case renderCall defaults (systemOne "hi" (pure ())) of
              Left (InvalidRequest NoQuestions) -> pure ()
              other -> assertFailure (show other)
        , testCase "listModels is a GET without a body" $
            fmap (\r -> (httpRequestMethod r, httpRequestPath r, httpRequestBody r, lookup "Content-Type" (httpRequestHeaders r))) (renderCall defaults listModels)
              `rightIs` ("GET", ["v1", "models"], Nothing, Nothing)
        , testCase "per-call headers win over client headers; protected headers are dropped" $
            fmap httpRequestHeaders
              ( renderCall
                  defaults {callDefaultHeaders = [("X-Team", "search"), ("X-Trace", "client"), ("Authorization", "Bearer stolen")]}
                  (withHeaders [("X-Trace", "call"), ("Accept", "text/html")] listModels)
              )
              `rightIs` [("Accept", "application/json"), ("X-Trace", "call"), ("X-Team", "search")]
        , testCase "a 2xx body that does not match the schema is a ResponseError" $
            case parseResponse listModels (HttpResponse status200 [("x-typesafe-request-id", "req_1")] "{\"models\": 3}") of
              Left (ResponseError e) -> do
                responseErrorRequestId e @?= Just "req_1"
                responseErrorEndpoint e @?= "GET /v1/models"
              other -> assertFailure (show other)
        , testCase "an error status is a ServiceError" $
            case parseResponse listModels (HttpResponse status422 [] "{\"detail\":[{\"loc\":[\"body\",\"state\"],\"msg\":\"Field required\",\"type\":\"missing\"}]}") of
              Left (ServiceError e) -> do
                apiErrorKind e @?= UnprocessableEntity
                apiErrorMessage e @?= Just "body.state: Field required"
              other -> assertFailure (show other)
        , testCase "a typed evaluation keeps the raw response" $ do
            let body =
                  "{\"model\":\"jev-1.13.0\",\"answers\":{\"q\":{\"type\":\"noul\",\"noul\":0.5},\"extra\":{\"type\":\"noul\",\"noul\":0.1}},\
                  \\"usage\":{\"input_tokens\":7,\"output_tokens\":2}}"
            case parseResponse (systemOne "hi" (ask "q" (noul "?"))) (HttpResponse status200 [] body) of
              Right e -> do
                evaluationAnswers e @?= Noul 0.5
                evaluationUsage e @?= Usage 7 2
                Map.keys (Wire.systemOneResponseAnswers (evaluationResponse e)) @?= ["extra", "q"]
              Left err -> assertFailure (show err)
        , testCase "systemOneRequest and decodeEvaluation work without a Call" $ do
            request <- either (assertFailure . show) pure (systemOneRequest jevLatest "hi" triage)
            Map.keys (Wire.systemOneRequestQuestions request) @?= ["department", "frustration", "is_urgent"]
            let response = Wire.SystemOneResponse "jev-1.13.0" sampleAnswers (Usage 1 1)
            fmap (choiceSelected . triageDepartment . evaluationAnswers) (decodeEvaluation triage Nothing response)
              @?= Right Billing
        ]
    ]
  where
    defaults = CallDefaults jevLatest []
    bodyOf = fmap (fmap LBS.unpack . httpRequestBody)
    bodyJson r = case r of
      Left e -> Left (show e)
      Right req -> maybe (Left "no body") eitherDecode (httpRequestBody req)
    lookupKey k o = KeyMap.lookup k (o :: KeyMap.KeyMap Value)
    rightIs :: (Eq a, Show a) => Either TypeSafeError a -> a -> Assertion
    rightIs r expected = case r of
      Right a -> a @?= expected
      Left e -> assertFailure (show e)
    toJSONList' :: [Text] -> Value
    toJSONList' = Array . foldMap pure . map String

-- | An answer type the SDK does not know about.
newtype Ranking = Ranking [Text]
  deriving stock (Show, Eq)

instance FromJSON Ranking where
  parseJSON = withObject "Ranking" $ \o -> Ranking <$> o .: "order"
