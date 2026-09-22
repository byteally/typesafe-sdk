{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TypeSafe.Error
-- Description : Everything that can go wrong, as values
-- Copyright   : (c) 2026 byteally
-- License     : BSD-3-Clause
--
-- Every failure the SDK reports is a 'TypeSafeError'. It separates four
-- situations that callers usually handle differently:
--
-- ['InvalidRequest'] the request was rejected locally and nothing was sent,
-- for example two questions share an id. This is a bug in the calling code.
--
-- ['ServiceError'] the API answered with an error status. 'apiErrorKind'
-- says which one ('Unauthorized', 'RateLimited', 'Overloaded', …).
--
-- ['ConnectionError'] no HTTP response arrived: DNS, TLS, a reset
-- connection, or a timeout.
--
-- ['ResponseError'] the API answered successfully, but the body did not match
-- what was asked, for example a choice outside the options you offered.
--
-- Transports built on "TypeSafe.Call" retry transient failures (see
-- "TypeSafe.Retry") before reporting them.
module TypeSafe.Error
  ( -- * Errors
    TypeSafeError (..)
  , renderTypeSafeError

    -- * Local request validation
  , RequestError (..)
  , QuestionProblem (..)

    -- * Error responses
  , ApiError (..)
  , ApiErrorKind (..)
  , apiErrorKindFor
  , apiErrorFromResponse
  , ErrorBody (..)
  , ErrorDetail (..)
  , parseErrorBody
  , RequestId (..)

    -- * Connection failures
  , ConnectionError (..)

    -- * Unusable responses
  , ResponseError (..)
  , ResponseProblem (..)
  , AnswerError (..)
  , AnswerProblem (..)
  ) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception (..), SomeException)
import Data.Aeson (FromJSON (..), Value (..), withObject, (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import GHC.Generics (Generic)
import Network.HTTP.Types (ResponseHeaders, Status (..))
import TypeSafe.Wire
  ( HTTPValidationError (..)
  , LocationSegment (..)
  , QuestionId (..)
  , ValidationError (..)
  )

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Network.HTTP.Types (status401, status422, status429)

-- | Any failure reported by the SDK.
--
-- 'displayException' renders a one-line, human-readable summary (see
-- 'renderTypeSafeError'). API keys are never part of an error.
data TypeSafeError
  = -- | The request was rejected before anything was sent.
    InvalidRequest !RequestError
  | -- | The API answered with an error status.
    ServiceError !ApiError
  | -- | No HTTP response was received.
    ConnectionError !ConnectionError
  | -- | A successful response that does not match the request.
    ResponseError !ResponseError
  deriving stock (Show, Generic)

instance Exception TypeSafeError where
  displayException = Text.unpack . renderTypeSafeError

-- | A one-line, human-readable description of an error.
--
-- >>> renderTypeSafeError (InvalidRequest (DuplicateQuestionId "is_urgent"))
-- "invalid request: two questions use the id \"is_urgent\""
renderTypeSafeError :: TypeSafeError -> Text
renderTypeSafeError = \case
  InvalidRequest e -> "invalid request: " <> renderRequestError e
  ServiceError e ->
    apiErrorEndpoint e
      <> " failed with HTTP "
      <> tshow (statusCode (apiErrorStatus e))
      <> " ("
      <> tshow (apiErrorKind e)
      <> ")"
      <> maybe "" (": " <>) (apiErrorMessage e)
      <> requestIdSuffix (apiErrorRequestId e)
  ConnectionError (ConnectionFailed endpoint cause) ->
    endpoint <> ": connection failed: " <> Text.pack (displayException cause)
  ConnectionError (ConnectionTimedOut endpoint) ->
    endpoint <> ": timed out waiting for a response"
  ResponseError e ->
    responseErrorEndpoint e
      <> ": unexpected response: "
      <> renderProblem (responseErrorProblem e)
      <> requestIdSuffix (responseErrorRequestId e)
  where
    requestIdSuffix = maybe "" (\(RequestId r) -> " [request id " <> r <> "]")
    renderProblem = \case
      MalformedBody msg -> Text.pack msg
      MismatchedAnswers errs -> Text.intercalate "; " (map renderAnswerError (NonEmpty.toList errs))

renderRequestError :: RequestError -> Text
renderRequestError = \case
  NoQuestions -> "a request needs at least one question"
  DuplicateQuestionId (QuestionId q) -> "two questions use the id " <> tshow q
  InvalidQuestion (QuestionId q) problem -> "question " <> tshow q <> ": " <> renderQuestionProblem problem
  where
    renderQuestionProblem = \case
      DuplicateOption o -> "two options are named " <> tshow o

renderAnswerError :: AnswerError -> Text
renderAnswerError (AnswerError (QuestionId q) problem) =
  "question " <> tshow q <> ": " <> case problem of
    MissingAnswer -> "no answer in the response"
    UnexpectedAnswerType expected actual -> "expected a " <> expected <> " answer, got " <> actual
    UnknownOption o -> "the answer mentions option " <> tshow o <> ", which was not offered"
    UnknownLevel l -> "the answer mentions level " <> tshow l <> ", which is outside the rubric"
    UndecodableAnswer msg -> msg

------------------------------------------------------------------------------
-- Request validation

-- | Why a request was rejected before sending.
data RequestError
  = -- | The request has no questions. The API requires at least one.
    NoQuestions
  | -- | Two questions share an id, so their answers could not be told apart.
    DuplicateQuestionId !QuestionId
  | -- | A question is malformed.
    InvalidQuestion !QuestionId !QuestionProblem
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | What is wrong with a single question.
data QuestionProblem
  = -- | Two Choice options map to the same name.
    DuplicateOption !Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

------------------------------------------------------------------------------
-- Error responses

-- | The value of the @x-typesafe-request-id@ response header. Include it when
-- you contact TypeSafe support about a request.
newtype RequestId = RequestId {unRequestId :: Text}
  deriving newtype (Eq, Ord, Show, IsString, NFData)

-- | An error response from the API.
data ApiError = ApiError
  { apiErrorKind :: !ApiErrorKind
  -- ^ What the status code means.
  , apiErrorStatus :: !Status
  -- ^ The HTTP status.
  , apiErrorMessage :: !(Maybe Text)
  -- ^ The server's explanation, when the body has one.
  , apiErrorBody :: !ErrorBody
  -- ^ The parsed response body.
  , apiErrorHeaders :: !ResponseHeaders
  -- ^ The response headers, including any @Retry-After@.
  , apiErrorRequestId :: !(Maybe RequestId)
  -- ^ The request id assigned by the API.
  , apiErrorEndpoint :: !Text
  -- ^ The method and path that failed, such as @POST \/v1\/systemone@.
  }
  deriving stock (Eq, Show, Generic)

-- | The meaning of an error status.
data ApiErrorKind
  = -- | 400: the request is invalid.
    BadRequest
  | -- | 401: the API key is invalid.
    Unauthorized
  | -- | 403: access is denied. The API also uses this status when no API key
    -- is sent.
    PermissionDenied
  | -- | 404: the resource does not exist. Check the base URL.
    NotFound
  | -- | 422: the body failed validation. The 'ErrorBody' says which field.
    UnprocessableEntity
  | -- | 429: the rate limit is exceeded. Retry after a delay.
    RateLimited
  | -- | 529: TypeSafe is temporarily overloaded. Retry after a delay.
    Overloaded
  | -- | Any other 5xx status.
    InternalServerError
  | -- | Any other status.
    UnexpectedStatus
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | Classify an error status.
--
-- >>> map apiErrorKindFor [status401, status422, status429, toEnum 529, toEnum 503, toEnum 418]
-- [Unauthorized,UnprocessableEntity,RateLimited,Overloaded,InternalServerError,UnexpectedStatus]
apiErrorKindFor :: Status -> ApiErrorKind
apiErrorKindFor s = case statusCode s of
  400 -> BadRequest
  401 -> Unauthorized
  403 -> PermissionDenied
  404 -> NotFound
  422 -> UnprocessableEntity
  429 -> RateLimited
  529 -> Overloaded
  c
    | c >= 500 && c < 600 -> InternalServerError
    | otherwise -> UnexpectedStatus

-- | Build an 'ApiError' from the endpoint (for example
-- @\"POST \/v1\/systemone\"@) and an error response.
apiErrorFromResponse :: Text -> Status -> ResponseHeaders -> LBS.ByteString -> ApiError
apiErrorFromResponse endpoint status headers body =
  ApiError
    { apiErrorKind = apiErrorKindFor status
    , apiErrorStatus = status
    , apiErrorMessage = errorBodyMessage parsed
    , apiErrorBody = parsed
    , apiErrorHeaders = headers
    , apiErrorRequestId = RequestId . decodeLenient <$> lookup "x-typesafe-request-id" headers
    , apiErrorEndpoint = endpoint
    }
  where
    parsed = parseErrorBody body

-- | The body of an error response.
data ErrorBody
  = -- | Field-level validation failures, sent with HTTP 422.
    BodyValidation !HTTPValidationError
  | -- | A structured error, such as
    -- @{\"detail\": {\"error_type\": \"authentication_error\", \"message\": …}}@.
    BodyDetail !ErrorDetail
  | -- | Some other JSON document.
    BodyJson !Value
  | -- | A body that is not JSON.
    BodyText !Text
  | -- | No body.
    BodyEmpty
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The @detail@ object of a structured error.
data ErrorDetail = ErrorDetail
  { errorDetailType :: !(Maybe Text)
  -- ^ A machine-readable category, such as @authentication_error@.
  , errorDetailMessage :: !(Maybe Text)
  -- ^ A human-readable explanation.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Parse an error response body.
--
-- >>> parseErrorBody "{\"detail\":{\"error_type\":\"authentication_error\",\"message\":\"Cannot authenticate with the server.\"}}"
-- BodyDetail (ErrorDetail {errorDetailType = Just "authentication_error", errorDetailMessage = Just "Cannot authenticate with the server."})
--
-- >>> parseErrorBody "{\"detail\":\"Not Found\"}"
-- BodyDetail (ErrorDetail {errorDetailType = Nothing, errorDetailMessage = Just "Not Found"})
--
-- >>> parseErrorBody "upstream connect error"
-- BodyText "upstream connect error"
parseErrorBody :: LBS.ByteString -> ErrorBody
parseErrorBody body
  | BS8.all (`elem` (" \t\r\n" :: String)) (LBS.toStrict body) = BodyEmpty
  | otherwise = case Aeson.decode' body of
      Nothing -> BodyText (decodeLenient (LBS.toStrict body))
      Just v -> fromMaybe (BodyJson v) (parseMaybe structured v)
  where
    structured = withObject "error" $ \o -> do
      detail <- o .:? "detail"
      case detail of
        Just (Array _) -> BodyValidation <$> parseJSON (Object o)
        Just (String msg) -> pure (BodyDetail (ErrorDetail Nothing (Just msg)))
        Just (Object d) -> BodyDetail <$> (ErrorDetail <$> d .:? "error_type" <*> d .:? "message")
        _ -> fail "not a structured error"

errorBodyMessage :: ErrorBody -> Maybe Text
errorBodyMessage = \case
  BodyDetail d -> errorDetailMessage d
  BodyValidation v -> renderValidation <$> (httpValidationErrorDetail v >>= listToMaybe)
  BodyText t -> Just (Text.take 500 t)
  BodyJson _ -> Nothing
  BodyEmpty -> Nothing
  where
    renderValidation e =
      Text.intercalate "." (map segment (validationErrorLoc e)) <> ": " <> validationErrorMsg e
    segment = \case
      LocationField f -> f
      LocationIndex i -> tshow i

------------------------------------------------------------------------------
-- Connection failures

-- | A request that got no HTTP response. The 'Text' is the endpoint, such as
-- @POST \/v1\/systemone@.
data ConnectionError
  = -- | The connection could not be made or broke: DNS, TLS, refused or reset.
    -- The exception comes from the transport; the SDK redacts credentials
    -- from it.
    ConnectionFailed !Text !SomeException
  | -- | No response arrived within the timeout.
    ConnectionTimedOut !Text
  deriving stock (Show, Generic)

------------------------------------------------------------------------------
-- Unusable responses

-- | A successful response that could not be used.
data ResponseError = UnexpectedResponse
  { responseErrorEndpoint :: !Text
  -- ^ The method and path, such as @POST \/v1\/systemone@.
  , responseErrorRequestId :: !(Maybe RequestId)
  -- ^ The request id assigned by the API.
  , responseErrorProblem :: !ResponseProblem
  -- ^ What is wrong.
  , responseErrorBody :: !LBS.ByteString
  -- ^ The raw response body.
  }
  deriving stock (Eq, Show, Generic)

-- | What is wrong with a successful response.
data ResponseProblem
  = -- | The body does not match the response schema. The message includes a
    -- JSON path to the offending value.
    MalformedBody !String
  | -- | The body is valid, but some answers do not fit their questions.
    MismatchedAnswers !(NonEmpty AnswerError)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | A problem with the answer to one question.
data AnswerError = AnswerError
  { answerErrorQuestion :: !QuestionId
  -- ^ The question whose answer is unusable.
  , answerErrorProblem :: !AnswerProblem
  -- ^ What is wrong with it.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | What is wrong with an answer.
data AnswerProblem
  = -- | The response has no answer for the question.
    MissingAnswer
  | -- | The answer is of a different type: expected, then actual.
    UnexpectedAnswerType !Text !Text
  | -- | The answer names a Choice option that was not offered.
    UnknownOption !Text
  | -- | The answer names a Score level outside the rubric.
    UnknownLevel !Int
  | -- | A custom decoder (see 'TypeSafe.Question.otherQuestion') rejected the
    -- answer.
    UndecodableAnswer !Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

------------------------------------------------------------------------------

decodeLenient :: BS8.ByteString -> Text
decodeLenient = Text.decodeUtf8With Text.lenientDecode

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
