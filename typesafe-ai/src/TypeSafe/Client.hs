{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : TypeSafe.Client
-- Description : A TypeSafe API client built on http-client
-- Copyright   : (c) 2026 byteally
-- License     : BSD-3-Clause
--
-- Sends 'Call's (see "TypeSafe.Call") to the TypeSafe API over
-- @http-client@, with TLS, connection reuse, timeouts and retries.
--
-- @
-- import TypeSafe
--
-- main :: IO ()
-- main = do
--   client <- 'newClientFromEnv'                -- reads TYPESAFE_API_KEY
--   result <- 'send' client $
--     'systemOne' \"Help! My payouts have been failing for 3 days.\"
--       ('ask' \"is_urgent\" ('noul' \"Does this convey urgency?\"))
--   print ('noulProbability' ('evaluationAnswers' result))
-- @
--
-- A 'Client' is immutable and thread-safe. Create one per application and
-- share it: its connection pool is what makes repeated calls fast.
--
-- = Timeouts and retries
--
-- Each attempt must complete within 'configTimeout' (10 seconds by default,
-- see 'withTimeout' to change it per call). Failed attempts are retried
-- according to 'configRetryPolicy', or the policy set with 'withRetryPolicy';
-- see "TypeSafe.Retry" for the defaults.
module TypeSafe.Client
  ( -- * Clients
    Client
  , newClient
  , newClientWith
  , newClientFromEnv
  , clientConfig

    -- * Sending calls
  , send
  , sendEither

    -- * Configuration
  , ClientConfig (..)
  , defaultClientConfig
  , clientConfigFromEnv
  , ApiKey
  , mkApiKey
  , ConfigError (..)

    -- ** Environment variables
  , apiKeyEnv
  , baseUrlEnv
  , defaultModelEnv

    -- * Logging
  , LogEvent (..)
  , LogStage (..)
  , renderLogEvent
  , stderrLogger

    -- * Identification
  , userAgent

    -- * Re-exports
  , Manager
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (..), throwIO, toException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.IO as Text
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Data.Version (showVersion)
import GHC.Clock (getMonotonicTime)
import GHC.Generics (Generic)
import Numeric (showFFloat)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), Manager)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (RequestHeaders, Status, statusCode, urlEncode)
import qualified Paths_typesafe_ai as Paths
import System.Environment (lookupEnv)
import System.IO (stderr)
import qualified System.Info as Info
import System.Random (randomRIO)
import System.Timeout (timeout)
import TypeSafe.Core

-- $setup
-- >>> :set -XOverloadedStrings

------------------------------------------------------------------------------
-- Configuration

-- | A validated API key. Its 'Show' instance never reveals the key.
newtype ApiKey = ApiKey BS.ByteString

instance Show ApiKey where
  show _ = "<api key>"

-- | Validate an API key. Surrounding whitespace, such as the newline at the
-- end of a key file, is removed; an empty key or one containing whitespace,
-- control or non-ASCII characters is rejected.
--
-- >>> mkApiKey "  ts_live_abc123\n"
-- Right <api key>
--
-- >>> mkApiKey "ts live"
-- Left (InvalidApiKey "the API key contains whitespace, control or non-ASCII characters")
mkApiKey :: Text -> Either ConfigError ApiKey
mkApiKey raw
  | Text.null key = Left (InvalidApiKey "the API key is empty")
  | Text.any (\c -> c < '!' || c > '~') key =
      Left (InvalidApiKey "the API key contains whitespace, control or non-ASCII characters")
  | otherwise = Right (ApiKey (Text.encodeUtf8 key))
  where
    key = Text.strip raw

-- | Why a client could not be configured.
data ConfigError
  = -- | No API key was given, and 'apiKeyEnv' is unset or blank.
    MissingApiKey
  | -- | The API key is malformed.
    InvalidApiKey !Text
  | -- | The base URL cannot be parsed: the URL and the reason.
    InvalidBaseUrl !Text !Text
  | -- | The timeout is not positive.
    InvalidTimeout !NominalDiffTime
  deriving stock (Eq, Show, Generic)

instance Exception ConfigError where
  displayException = \case
    MissingApiKey -> "no TypeSafe API key: set " <> Text.unpack apiKeyEnv <> " or pass one to mkApiKey"
    InvalidApiKey reason -> "invalid TypeSafe API key: " <> Text.unpack reason
    InvalidBaseUrl url reason -> "invalid TypeSafe base URL " <> show url <> ": " <> Text.unpack reason
    InvalidTimeout t -> "invalid timeout " <> show t <> ": it must be positive"

-- | Client settings. Start from 'defaultClientConfig' or
-- 'clientConfigFromEnv' and override fields with record update syntax:
--
-- @
-- (defaultClientConfig key) {'configTimeout' = Just 30, 'configLogger' = 'stderrLogger'}
-- @
data ClientConfig = ClientConfig
  { configApiKey :: !ApiKey
  -- ^ Sent as @Authorization: Bearer …@.
  , configBaseUrl :: !Text
  -- ^ The API root, 'defaultBaseUrl' by default. It may include a path
  -- prefix, for example to go through an AI gateway.
  , configDefaultModel :: !ModelName
  -- ^ The model for calls that do not set one with 'withModel'.
  , configRetryPolicy :: !RetryPolicy
  -- ^ How failed attempts are retried.
  , configTimeout :: !(Maybe NominalDiffTime)
  -- ^ How long each attempt may take, in seconds. 'Nothing' waits forever.
  , configHeaders :: !RequestHeaders
  -- ^ Extra headers for every request. The 'protectedHeaders' cannot be
  -- overridden.
  , configLogger :: !(LogEvent -> IO ())
  -- ^ Called for every attempt, retry and result. Plug in your logging
  -- library here; the default discards everything. Events never contain the
  -- API key or request bodies.
  }

-- | The defaults: 'defaultBaseUrl', 'jevLatest', 'defaultRetryPolicy', a
-- 10 second timeout, no extra headers and no logging.
defaultClientConfig :: ApiKey -> ClientConfig
defaultClientConfig key =
  ClientConfig
    { configApiKey = key
    , configBaseUrl = defaultBaseUrl
    , configDefaultModel = jevLatest
    , configRetryPolicy = defaultRetryPolicy
    , configTimeout = Just 10
    , configHeaders = []
    , configLogger = \_ -> pure ()
    }

-- | @TYPESAFE_API_KEY@: the API key. Required by 'clientConfigFromEnv'.
apiKeyEnv :: Text
apiKeyEnv = "TYPESAFE_API_KEY"

-- | @TYPESAFE_BASE_URL@: overrides 'configBaseUrl'.
baseUrlEnv :: Text
baseUrlEnv = "TYPESAFE_BASE_URL"

-- | @TYPESAFE_DEFAULT_MODEL@: overrides 'configDefaultModel'.
defaultModelEnv :: Text
defaultModelEnv = "TYPESAFE_DEFAULT_MODEL"

-- | 'defaultClientConfig' with the API key, base URL and default model read
-- from 'apiKeyEnv', 'baseUrlEnv' and 'defaultModelEnv'. Blank variables count
-- as unset. These are the same variables the official Python and JavaScript
-- SDKs read.
clientConfigFromEnv :: IO (Either ConfigError ClientConfig)
clientConfigFromEnv = do
  key <- env apiKeyEnv
  baseUrl <- env baseUrlEnv
  model <- env defaultModelEnv
  pure $ do
    apiKey <- maybe (Left MissingApiKey) mkApiKey key
    let config = defaultClientConfig apiKey
    pure
      config
        { configBaseUrl = fromMaybe (configBaseUrl config) baseUrl
        , configDefaultModel = maybe (configDefaultModel config) ModelName model
        }
  where
    env name = do
      value <- lookupEnv (Text.unpack name)
      pure $ case Text.strip . Text.pack <$> value of
        Just v | not (Text.null v) -> Just v
        _ -> Nothing

------------------------------------------------------------------------------
-- Clients

-- | A configured connection to the TypeSafe API.
data Client = Client
  { clientConfig_ :: !ClientConfig
  , clientManager :: !Manager
  , clientBaseRequest :: !HTTP.Request
  }

-- | The configuration a client was created with.
clientConfig :: Client -> ClientConfig
clientConfig = clientConfig_

-- | Create a client with its own TLS connection pool. Throws 'ConfigError'
-- if the configuration is invalid.
newClient :: ClientConfig -> IO Client
newClient config = do
  manager <- newTlsManager
  either throwIO pure (newClientWith manager config)

-- | Create a client that shares an existing @http-client@ 'Manager', for
-- example with servant-client or your own HTTP code. The manager must
-- support TLS for @https@ URLs (see "Network.HTTP.Client.TLS").
newClientWith :: Manager -> ClientConfig -> Either ConfigError Client
newClientWith manager config = do
  case configTimeout config of
    Just t | t <= 0 -> Left (InvalidTimeout t)
    _ -> Right ()
  base <- parseBaseUrl (configBaseUrl config)
  pure Client {clientConfig_ = config, clientManager = manager, clientBaseRequest = base}

-- | 'newClient' with 'clientConfigFromEnv'. Throws 'ConfigError' if
-- @TYPESAFE_API_KEY@ is not set or a variable is invalid.
newClientFromEnv :: IO Client
newClientFromEnv = clientConfigFromEnv >>= either throwIO newClient

parseBaseUrl :: Text -> Either ConfigError HTTP.Request
parseBaseUrl url = case HTTP.parseRequest (Text.unpack url) of
  Left e -> Left (InvalidBaseUrl url (Text.pack (reason e)))
  Right r
    | not (BS.null (HTTP.queryString r)) || BS8.elem '#' (HTTP.path r) ->
        Left (InvalidBaseUrl url "the base URL must not have a query string or fragment")
    | otherwise -> Right r
  where
    reason e = case fromException e of
      Just (InvalidUrlException _ why) -> why
      _ -> displayException e

------------------------------------------------------------------------------
-- Sending

-- | Send a call and return its result. Throws 'TypeSafeError' once retries
-- are exhausted.
send :: Client -> Call a -> IO a
send client call = sendEither client call >>= either throwIO pure

-- | Send a call and return its result or the error, after any retries.
-- Exceptions that are not about the request, such as asynchronous
-- exceptions, propagate as usual.
sendEither :: Client -> Call a -> IO (Either TypeSafeError a)
sendEither client call = case renderCall defaults call of
  Left err -> do
    logEvent 0 (Failed err)
    pure (Left err)
  Right request -> do
    start <- getMonotonicTime
    attempt start (toHttpRequest client request) 1
  where
    config = clientConfig_ client
    defaults = CallDefaults (configDefaultModel config) (configHeaders config)
    policy = fromMaybe (configRetryPolicy config) (callRetryPolicy call)
    limit = maybe (configTimeout config) Just (callTimeout call)
    endpoint = callEndpoint call
    logEvent n stage = configLogger config (LogEvent endpoint n stage)

    attempt start request n = do
      logEvent n Sending
      began <- getMonotonicTime
      outcome <- exchange request
      finished <- getMonotonicTime
      let latency = realToFrac (finished - began)
      result <- case outcome of
        Left err -> pure (Left err)
        Right response -> do
          logEvent n (Received (HTTP.responseStatus response) latency (requestIdOf response))
          pure (parseResponse call (fromHttpResponse response))
      case result of
        Right a -> pure (Right a)
        Left err
          | n <= retryMaxRetries policy && isRetryable policy err -> do
              now <- getCurrentTime
              random <- randomRIO (0, 1)
              let delay = retryDelay policy now random n err
              elapsed <- subtract start <$> getMonotonicTime
              if maybe False (\budget -> realToFrac elapsed + delay >= budget) (retryBudget policy)
                then giveUp n err
                else do
                  logEvent n (Retrying delay err)
                  threadDelay (microseconds delay)
                  attempt start request (n + 1)
          | otherwise -> giveUp n err

    giveUp n err = do
      logEvent n (Failed err)
      pure (Left err)

    exchange request = do
      let run = try (HTTP.httpLbs request (clientManager client))
      outcome <- case limit of
        Nothing -> Just <$> run
        Just t -> timeout (microseconds t) run
      pure $ case outcome of
        Nothing -> Left (ConnectionError (ConnectionTimedOut endpoint))
        Just (Left e) -> Left (classify e)
        Just (Right response) -> Right response

    classify :: HttpException -> TypeSafeError
    classify = \case
      HttpExceptionRequest _ ResponseTimeout -> ConnectionError (ConnectionTimedOut endpoint)
      HttpExceptionRequest _ ConnectionTimeout -> ConnectionError (ConnectionTimedOut endpoint)
      HttpExceptionRequest r content ->
        ConnectionError (ConnectionFailed endpoint (toException (HttpExceptionRequest (redact r) content)))
      e -> ConnectionError (ConnectionFailed endpoint (toException e))

    redact r =
      r
        { HTTP.requestHeaders =
            [(name, if name == "Authorization" then "<redacted>" else value) | (name, value) <- HTTP.requestHeaders r]
        }

    requestIdOf response = RequestId . Text.decodeUtf8With Text.lenientDecode <$> lookup requestIdHeader (HTTP.responseHeaders response)

toHttpRequest :: Client -> HttpRequest -> HTTP.Request
toHttpRequest client r =
  base
    { HTTP.method = httpRequestMethod r
    , HTTP.path = basePath <> "/" <> BS.intercalate "/" (map (urlEncode False . Text.encodeUtf8) (httpRequestPath r))
    , HTTP.requestHeaders =
        ("Authorization", "Bearer " <> key)
          : ("User-Agent", userAgent)
          : httpRequestHeaders r
    , HTTP.requestBody = maybe mempty HTTP.RequestBodyLBS (httpRequestBody r)
    , HTTP.responseTimeout = HTTP.responseTimeoutNone
    }
  where
    base = clientBaseRequest client
    basePath = BS8.dropWhileEnd (== '/') (HTTP.path base)
    ApiKey key = configApiKey (clientConfig_ client)

fromHttpResponse :: HTTP.Response LBS.ByteString -> HttpResponse
fromHttpResponse response =
  HttpResponse (HTTP.responseStatus response) (HTTP.responseHeaders response) (HTTP.responseBody response)

microseconds :: NominalDiffTime -> Int
microseconds t = max 1 (ceiling (t * 1000000))

-- | The @User-Agent@ sent with every request, such as
-- @typesafe-ai-haskell\/0.1.0.0 (ghc-9.12)@.
userAgent :: BS.ByteString
userAgent =
  BS8.pack $
    "typesafe-ai-haskell/"
      <> showVersion Paths.version
      <> " ("
      <> Info.compilerName
      <> "-"
      <> showVersion Info.compilerVersion
      <> ")"

------------------------------------------------------------------------------
-- Logging

-- | Something that happened while sending a call.
data LogEvent = LogEvent
  { logEndpoint :: !Text
  -- ^ The method and path, such as @POST \/v1\/systemone@.
  , logAttempt :: !Int
  -- ^ The attempt number, from 1; 0 for calls rejected before sending.
  , logStage :: !LogStage
  }
  deriving stock (Show, Generic)

-- | The stages of an attempt.
data LogStage
  = -- | The request is about to be sent.
    Sending
  | -- | A response arrived: its status, how long it took, and its request id.
    Received !Status !NominalDiffTime !(Maybe RequestId)
  | -- | The attempt failed and will be retried after the delay.
    Retrying !NominalDiffTime !TypeSafeError
  | -- | The call failed for good.
    Failed !TypeSafeError
  deriving stock (Show, Generic)

-- | A one-line description of an event.
renderLogEvent :: LogEvent -> Text
renderLogEvent (LogEvent endpoint n stage) =
  "[typesafe] " <> endpoint <> " (attempt " <> tshow n <> "): " <> case stage of
    Sending -> "sending"
    Received status latency rid ->
      "HTTP "
        <> tshow (statusCode status)
        <> " in "
        <> seconds latency
        <> maybe "" (\(RequestId r) -> " [" <> r <> "]") rid
    Retrying delay err -> "retrying in " <> seconds delay <> " after: " <> renderTypeSafeError err
    Failed err -> "failed: " <> renderTypeSafeError err

  where
    seconds t = Text.pack (showFFloat (Just 3) (realToFrac t :: Double) "s")

-- | A logger that prints every event to standard error, for debugging.
stderrLogger :: LogEvent -> IO ()
stderrLogger = Text.hPutStrLn stderr . renderLogEvent

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
