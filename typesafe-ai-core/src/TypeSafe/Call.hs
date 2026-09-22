{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TypeSafe.Call
-- Description : API calls as values, independent of any HTTP library
-- Copyright   : (c) 2026 byteally
-- License     : BSD-3-Clause
--
-- A @'Call' a@ describes one API request and how to turn its response into
-- an @a@. It does no input or output: a /transport/ renders the call to an
-- 'HttpRequest', sends it with whatever HTTP stack it likes, and hands the
-- 'HttpResponse' back to 'parseResponse'. The @typesafe-ai@ package provides
-- a transport built on @http-client@; writing another one takes a few dozen
-- lines.
--
-- Calls are built from the endpoint functions and adjusted with the @with…@
-- functions:
--
-- @
-- 'withModel' \"jev-1.13.0\" ('systemOne' state triage) :: 'Call' ('Evaluation' Triage)
-- @
--
-- = Example
--
-- >>> let call = systemOne "I was charged twice. Please help." (ask "billing" (noul "Is this about billing?"))
-- >>> callEndpoint call
-- "POST /v1/systemone"
--
-- >>> let Right request = renderCall (CallDefaults jevLatest []) call
-- >>> traverse_ LBS.putStrLn (httpRequestBody request)
-- {"state":"I was charged twice. Please help.","model":"jev-latest","questions":{"billing":{"type":"noul","instructions":"Is this about billing?"}}}
--
-- >>> :{
-- let response =
--       HttpResponse
--         { httpResponseStatus = status200
--         , httpResponseHeaders = [("x-typesafe-request-id", "req_0123")]
--         , httpResponseBody = "{\"model\":\"jev-1.13.0\",\"answers\":{\"billing\":{\"type\":\"noul\",\"noul\":0.98}},\"usage\":{\"input_tokens\":120,\"output_tokens\":12}}"
--         }
-- :}
--
-- >>> let Right result = parseResponse call response
-- >>> evaluationAnswers result
-- Noul {noulProbability = 0.98}
-- >>> evaluationModel result
-- "jev-1.13.0"
-- >>> evaluationRequestId result
-- Just "req_0123"
module TypeSafe.Call
  ( -- * Calls
    Call
  , systemOne
  , systemOneRaw
  , listModels

    -- * Evaluations
  , Evaluation (..)

    -- * Per-call options
  , withModel
  , withHeaders
  , withExtraBody
  , withRetryPolicy
  , withTimeout

    -- * Using the typed layer with your own HTTP client
  , systemOneRequest
  , decodeEvaluation

    -- * Implementing a transport
  , CallDefaults (..)
  , HttpRequest (..)
  , HttpResponse (..)
  , renderCall
  , parseResponse
  , callEndpoint
  , callRetryPolicy
  , callTimeout
  , protectedHeaders
  , defaultBaseUrl
  , requestIdHeader
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (Object, eitherDecode', pairs, (.=))
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Data.Time.Clock (NominalDiffTime)
import GHC.Generics (Generic)
import Network.HTTP.Types
  ( HeaderName
  , Method
  , RequestHeaders
  , ResponseHeaders
  , Status
  , statusIsSuccessful
  )
import TypeSafe.Content (Content)
import TypeSafe.Error
  ( AnswerError
  , RequestError
  , RequestId (..)
  , ResponseError (..)
  , ResponseProblem (..)
  , TypeSafeError (..)
  , apiErrorFromResponse
  )
import TypeSafe.Question (Questions, decodeAnswers, renderQuestions)
import TypeSafe.Retry (RetryPolicy)
import TypeSafe.Wire
  ( Answer
  , ModelMetadata
  , ModelMetadataList (..)
  , ModelName
  , QuestionId
  , SystemOneRequest (..)
  , SystemOneResponse (..)
  , Usage
  )

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Foldable (traverse_)
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS
-- >>> import Network.HTTP.Types (status200)
-- >>> import TypeSafe.Question
-- >>> import TypeSafe.Wire (jevLatest)

------------------------------------------------------------------------------
-- Calls

-- | One API request, and how to decode its response into an @a@.
data Call a = Call
  { callMethod :: !Method
  , callPath :: ![Text]
  , callBody :: !(Maybe (CallOptions -> ModelName -> Either RequestError LBS.ByteString))
  , callOptions :: !CallOptions
  , callDecode :: Maybe RequestId -> LBS.ByteString -> Either ResponseProblem a
  }

instance Functor Call where
  fmap f c = c {callDecode = \rid body -> f <$> callDecode c rid body}

data CallOptions = CallOptions
  { optionModel :: !(Maybe ModelName)
  , optionHeaders :: !RequestHeaders
  , optionExtraBody :: !Object
  , optionRetryPolicy :: !(Maybe RetryPolicy)
  , optionTimeout :: !(Maybe NominalDiffTime)
  }

noOptions :: CallOptions
noOptions = CallOptions Nothing [] KeyMap.empty Nothing Nothing

-- | Evaluate a state against typed questions (@POST \/v1\/systemone@).
--
-- The request uses the transport's default model (normally 'TypeSafe.Wire.jevLatest')
-- unless 'withModel' says otherwise. Every question sees the same state and
-- is answered independently; see <https://docs.typesafe.ai/api>.
--
-- The call fails with 'InvalidRequest', before anything is sent, if the
-- questions are empty, share an id, or are malformed.
systemOne :: Content -> Questions a -> Call (Evaluation a)
systemOne state questions =
  Call
    { callMethod = "POST"
    , callPath = ["v1", "systemone"]
    , callBody = Just $ \options defaultModel -> do
        rendered <- renderQuestions questions
        pure . encodeRequest options $
          SystemOneRequest
            { systemOneRequestState = state
            , systemOneRequestModel = fromMaybe defaultModel (optionModel options)
            , systemOneRequestQuestions = rendered
            }
    , callOptions = noOptions
    , callDecode = \rid body -> do
        response <- first MalformedBody (eitherDecode' body)
        first MismatchedAnswers (decodeEvaluation questions rid response)
    }

-- | Send a wire-level request as-is and receive the raw answers.
--
-- Nothing is validated locally. The model is the one in the request unless
-- 'withModel' overrides it.
systemOneRaw :: SystemOneRequest -> Call (Evaluation (Map QuestionId Answer))
systemOneRaw request =
  Call
    { callMethod = "POST"
    , callPath = ["v1", "systemone"]
    , callBody = Just $ \options _ ->
        Right . encodeRequest options $
          request
            { systemOneRequestModel =
                fromMaybe (systemOneRequestModel request) (optionModel options)
            }
    , callOptions = noOptions
    , callDecode = \rid body -> do
        response <- first MalformedBody (eitherDecode' body)
        pure (evaluation rid response (systemOneResponseAnswers response))
    }

-- | List the models and aliases available to the account
-- (@GET \/v1\/models@).
--
-- Versioned model ids are accepted by the @model@ field whether or not they
-- are listed.
listModels :: Call [ModelMetadata]
listModels =
  Call
    { callMethod = "GET"
    , callPath = ["v1", "models"]
    , callBody = Nothing
    , callOptions = noOptions
    , callDecode = \_ body -> modelMetadataListModels <$> first MalformedBody (eitherDecode' body)
    }

encodeRequest :: CallOptions -> SystemOneRequest -> LBS.ByteString
encodeRequest options r =
  Encoding.encodingToLazyByteString . pairs $
    "state" .= systemOneRequestState r
      <> "model" .= systemOneRequestModel r
      <> "questions" .= systemOneRequestQuestions r
      <> foldMap
        (uncurry (.=))
        [ kv
        | kv@(k, _) <- KeyMap.toList (optionExtraBody options)
        , k `notElem` ["state", "model", "questions"]
        ]

------------------------------------------------------------------------------
-- Evaluations

-- | The result of a 'systemOne' call: your typed answers, plus what the API
-- reported about the request.
data Evaluation a = Evaluation
  { evaluationAnswers :: !a
  -- ^ The decoded answers.
  , evaluationModel :: !ModelName
  -- ^ The versioned model that answered. Log it to know which model
  -- produced a result when you request an alias.
  , evaluationUsage :: !Usage
  -- ^ Token usage.
  , evaluationRequestId :: !(Maybe RequestId)
  -- ^ The request id assigned by the API.
  , evaluationResponse :: !SystemOneResponse
  -- ^ The complete response, including answers the typed questions did not
  -- ask for.
  }
  deriving stock (Eq, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

evaluation :: Maybe RequestId -> SystemOneResponse -> a -> Evaluation a
evaluation rid response a =
  Evaluation
    { evaluationAnswers = a
    , evaluationModel = systemOneResponseModel response
    , evaluationUsage = systemOneResponseUsage response
    , evaluationRequestId = rid
    , evaluationResponse = response
    }

-- | Build the wire request for typed questions, for sending with your own
-- HTTP client (servant, webapi, …). Pair it with 'decodeEvaluation'.
systemOneRequest :: ModelName -> Content -> Questions a -> Either RequestError SystemOneRequest
systemOneRequest model state questions =
  SystemOneRequest state model <$> renderQuestions questions

-- | Decode a response to a request built with 'systemOneRequest'. The request
-- id is the @x-typesafe-request-id@ response header, if you have it.
decodeEvaluation
  :: Questions a
  -> Maybe RequestId
  -> SystemOneResponse
  -> Either (NonEmpty AnswerError) (Evaluation a)
decodeEvaluation questions rid response =
  evaluation rid response <$> decodeAnswers questions (systemOneResponseAnswers response)

------------------------------------------------------------------------------
-- Options

-- | Use this model (a versioned id or an alias) instead of the default.
withModel :: ModelName -> Call a -> Call a
withModel m = overOptions (\o -> o {optionModel = Just m})

-- | Send extra headers with this call. They override headers of the same name
-- configured on the client, but never the 'protectedHeaders'.
withHeaders :: RequestHeaders -> Call a -> Call a
withHeaders hs = overOptions (\o -> o {optionHeaders = optionHeaders o <> hs})

-- | Add top-level fields to the request body, to use API features that this
-- SDK does not know about yet. Fields the SDK sets itself (@state@, @model@,
-- @questions@) cannot be overridden this way. Only send fields the API
-- supports.
withExtraBody :: Object -> Call a -> Call a
withExtraBody extra = overOptions (\o -> o {optionExtraBody = extra <> optionExtraBody o})

-- | Retry this call with a different policy than the client's.
withRetryPolicy :: RetryPolicy -> Call a -> Call a
withRetryPolicy p = overOptions (\o -> o {optionRetryPolicy = Just p})

-- | Wait at most this many seconds for each attempt of this call.
withTimeout :: NominalDiffTime -> Call a -> Call a
withTimeout t = overOptions (\o -> o {optionTimeout = Just t})

overOptions :: (CallOptions -> CallOptions) -> Call a -> Call a
overOptions f c = c {callOptions = f (callOptions c)}

------------------------------------------------------------------------------
-- Transports

-- | Client-wide settings a transport supplies when rendering a call.
data CallDefaults = CallDefaults
  { callDefaultModel :: !ModelName
  -- ^ The model used unless the call sets one.
  , callDefaultHeaders :: !RequestHeaders
  -- ^ Extra headers for every call.
  }
  deriving stock (Eq, Show)

-- | An HTTP request, ready to send.
--
-- The transport adds the base URL, the @Authorization: Bearer …@ header and
-- its @User-Agent@.
data HttpRequest = HttpRequest
  { httpRequestMethod :: !Method
  , httpRequestPath :: ![Text]
  -- ^ Path segments, relative to the base URL.
  , httpRequestHeaders :: !RequestHeaders
  , httpRequestBody :: !(Maybe LBS.ByteString)
  -- ^ A JSON body, if the call has one.
  }
  deriving stock (Eq, Show)

-- | An HTTP response, as received by the transport.
data HttpResponse = HttpResponse
  { httpResponseStatus :: !Status
  , httpResponseHeaders :: !ResponseHeaders
  , httpResponseBody :: !LBS.ByteString
  }
  deriving stock (Eq, Show)

-- | Render a call, or report why it cannot be sent.
renderCall :: CallDefaults -> Call a -> Either TypeSafeError HttpRequest
renderCall defaults call = do
  body <-
    traverse
      (\encode -> first InvalidRequest (encode options (callDefaultModel defaults)))
      (callBody call)
  pure
    HttpRequest
      { httpRequestMethod = callMethod call
      , httpRequestPath = callPath call
      , httpRequestHeaders =
          ("Accept", "application/json")
            : [("Content-Type", "application/json") | isJust body]
              <> overriding (optionHeaders options) (callDefaultHeaders defaults)
      , httpRequestBody = body
      }
  where
    options = callOptions call
    overriding preferred fallback =
      filter
        ((`notElem` protectedHeaders) . fst)
        (preferred <> [h | h@(name, _) <- fallback, name `notElem` map fst preferred])

-- | Decode a response: the call's result for a 2xx status, a 'ServiceError'
-- otherwise.
parseResponse :: Call a -> HttpResponse -> Either TypeSafeError a
parseResponse call response
  | statusIsSuccessful status =
      first
        (\problem -> ResponseError (UnexpectedResponse (callEndpoint call) rid problem body))
        (callDecode call rid body)
  | otherwise =
      Left (ServiceError (apiErrorFromResponse (callEndpoint call) status headers body))
  where
    status = httpResponseStatus response
    headers = httpResponseHeaders response
    body = httpResponseBody response
    rid = RequestId . Text.decodeUtf8With Text.lenientDecode <$> lookup requestIdHeader headers

-- | The method and path of a call, such as @POST \/v1\/systemone@, for logs
-- and error messages.
callEndpoint :: Call a -> Text
callEndpoint c =
  Text.decodeUtf8With Text.lenientDecode (callMethod c) <> " /" <> Text.intercalate "/" (callPath c)

-- | The retry policy set with 'withRetryPolicy', if any.
callRetryPolicy :: Call a -> Maybe RetryPolicy
callRetryPolicy = optionRetryPolicy . callOptions

-- | The per-attempt timeout set with 'withTimeout', if any.
callTimeout :: Call a -> Maybe NominalDiffTime
callTimeout = optionTimeout . callOptions

-- | Headers that only the SDK sets: authentication, content negotiation and
-- SDK identification. Extra headers with these names are dropped.
protectedHeaders :: [HeaderName]
protectedHeaders = ["Authorization", "Accept", "Content-Type", "Content-Length", "User-Agent"]

-- | @https:\/\/api.typesafe.ai@
defaultBaseUrl :: Text
defaultBaseUrl = "https://api.typesafe.ai"

-- | The response header that carries the request id.
requestIdHeader :: HeaderName
requestIdHeader = "x-typesafe-request-id"
