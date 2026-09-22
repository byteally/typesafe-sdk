{-# LANGUAGE OverloadedStrings #-}

-- | Tests the http-client transport against a local mock of the API.
--
-- Set @TYPESAFE_LIVE_TESTS=1@ and @TYPESAFE_API_KEY@ to also run a few
-- requests against the real API.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TypeSafe

-- | A request as the mock server saw it.
data Seen = Seen
  { seenMethod :: Method
  , seenPath :: [Text.Text]
  , seenHeaders :: RequestHeaders
  , seenBody :: LBS.ByteString
  }

-- | Run a mock API that answers requests with the given responses in order,
-- repeating the last one.
withMockApi :: [IO Wai.Response] -> (Int -> IORef [Seen] -> IO a) -> IO a
withMockApi responses action = do
  seen <- newIORef []
  script <- newIORef responses
  let app request respond = do
        body <- Wai.strictRequestBody request
        modifyIORef' seen (<> [Seen (Wai.requestMethod request) (Wai.pathInfo request) (Wai.requestHeaders request) body])
        next <- atomicModifyIORef' script $ \pending -> case pending of
          [r] -> ([r], r)
          r : rs -> (rs, r)
          [] -> ([], pure (Wai.responseLBS status500 [] "no scripted response"))
        next >>= respond
  testWithApplication (pure app) (\port -> action port seen)

json :: Status -> ResponseHeaders -> LBS.ByteString -> IO Wai.Response
json status headers body = pure (Wai.responseLBS status (("Content-Type", "application/json") : headers) body)

okNoul :: IO Wai.Response
okNoul =
  json
    status200
    [("x-typesafe-request-id", "req_mock")]
    "{\"model\":\"jev-1.13.0\",\"answers\":{\"billing\":{\"type\":\"noul\",\"noul\":0.98}},\"usage\":{\"input_tokens\":120,\"output_tokens\":12}}"

billing :: Call (Evaluation Noul)
billing = systemOne "I was charged twice." (ask "billing" (noul "Is this about billing?"))

testKey :: ApiKey
testKey = either (error . show) id (mkApiKey "ts_test_secret_value")

-- | A client for the mock API, with fast retries, recording log events.
mockClient :: Int -> (ClientConfig -> ClientConfig) -> IO (Client, IORef [LogEvent])
mockClient port adjust = do
  events <- newIORef []
  manager <- newManager defaultManagerSettings
  let config =
        adjust
          (defaultClientConfig testKey)
            { configBaseUrl = "http://127.0.0.1:" <> Text.pack (show port)
            , configRetryPolicy =
                defaultRetryPolicy {retryInitialBackoff = 0.01, retryMaxBackoff = 0.02}
            , configLogger = \e -> modifyIORef' events (<> [e])
            }
  client <- either (fail . show) pure (newClientWith manager config)
  pure (client, events)

attempts :: IORef [Seen] -> IO Int
attempts seen = length <$> readIORef seen

main :: IO ()
main = do
  live <- liveTests
  defaultMain (testGroup "typesafe-ai" (transportTests : configTests : live))

transportTests :: TestTree
transportTests =
  testGroup
    "http-client transport"
    [ testCase "sends a typed call and decodes the answer" $
        withMockApi [okNoul] $ \port seen -> do
          (client, _) <- mockClient port id
          result <- send client billing
          evaluationAnswers result @?= Noul 0.98
          evaluationRequestId result @?= Just "req_mock"
          [request] <- readIORef seen
          seenMethod request @?= "POST"
          seenPath request @?= ["v1", "systemone"]
          Aeson.decode (seenBody request)
            @?= Just
              ( Aeson.object
                  [ "state" Aeson..= ("I was charged twice." :: Text.Text)
                  , "model" Aeson..= ("jev-latest" :: Text.Text)
                  , "questions" Aeson..= Aeson.object ["billing" Aeson..= Aeson.object ["type" Aeson..= ("noul" :: Text.Text), "instructions" Aeson..= ("Is this about billing?" :: Text.Text)]]
                  ]
              )
    , testCase "authenticates, identifies itself and negotiates JSON" $
        withMockApi [okNoul] $ \port seen -> do
          (client, _) <- mockClient port id
          _ <- send client billing
          [request] <- readIORef seen
          let header name = lookup name (seenHeaders request)
          header "Authorization" @?= Just "Bearer ts_test_secret_value"
          header "Content-Type" @?= Just "application/json"
          header "Accept" @?= Just "application/json"
          assertBool "user agent" (maybe False ("typesafe-ai-haskell/" `LBS.isPrefixOf`) (LBS.fromStrict <$> header "User-Agent"))
    , testCase "extra headers cannot replace the protected ones" $
        withMockApi [okNoul] $ \port seen -> do
          (client, _) <- mockClient port (\c -> c {configHeaders = [("X-Team", "search"), ("Authorization", "Bearer other")]})
          _ <- send client (withHeaders [("User-Agent", "spoof"), ("X-Call", "1")] billing)
          [request] <- readIORef seen
          let values name = [v | (n, v) <- seenHeaders request, n == name]
          values "Authorization" @?= ["Bearer ts_test_secret_value"]
          values "User-Agent" @?= [userAgent]
          values "X-Team" @?= ["search"]
          values "X-Call" @?= ["1"]
    , testCase "a base URL with a path prefix is kept" $
        withMockApi [okNoul] $ \port seen -> do
          (client, _) <- mockClient port (\c -> c {configBaseUrl = configBaseUrl c <> "/gateway/typesafe/"})
          _ <- send client billing
          map seenPath <$> readIORef seen >>= (@?= [["gateway", "typesafe", "v1", "systemone"]])
    , testCase "lists models" $
        withMockApi [json status200 [] "{\"models\":[{\"name\":\"jev-latest\",\"description\":\"General-purpose system one model.\",\"release_date\":\"2026-09-15\"}]}"] $
          \port seen -> do
            (client, _) <- mockClient port id
            models <- send client listModels
            map modelMetadataName models @?= ["jev-latest"]
            map modelMetadataReleaseDay models @?= [Just (fromGregorian 2026 9 15)]
            [request] <- readIORef seen
            (seenMethod request, seenBody request) @?= ("GET", "")
    , testCase "retries a rate limit, honouring retry-after-ms" $
        withMockApi [json status429 [("retry-after-ms", "30")] "{}", okNoul] $ \port seen -> do
          (client, events) <- mockClient port id
          result <- sendEither client billing
          fmap evaluationAnswers result `rightIs` Noul 0.98
          attempts seen >>= (@?= 2)
          delays <- mapRetries <$> readIORef events
          delays @?= [0.03]
    , testCase "retries an overloaded API until the retries run out" $
        withMockApi [json (mkStatus 529 "Overloaded") [] "{\"detail\":{\"message\":\"busy\"}}"] $ \port seen -> do
          (client, events) <- mockClient port id
          result <- sendEither client billing
          case result of
            Left (ServiceError e) -> (apiErrorKind e, apiErrorMessage e) @?= (Overloaded, Just "busy")
            other -> assertFailure (show (fmap evaluationAnswers other))
          attempts seen >>= (@?= 3)
          stages <- map logStage <$> readIORef events
          length [() | Retrying _ _ <- stages] @?= 2
          length [() | Failed _ <- stages] @?= 1
    , testCase "does not retry a validation error" $
        withMockApi [json status422 [] "{\"detail\":[{\"loc\":[\"body\",\"state\"],\"msg\":\"Field required\",\"type\":\"missing\"}]}"] $
          \port seen -> do
            (client, _) <- mockClient port id
            result <- sendEither client billing
            case result of
              Left (ServiceError e) -> apiErrorKind e @?= UnprocessableEntity
              other -> assertFailure (show (fmap evaluationAnswers other))
            attempts seen >>= (@?= 1)
    , testCase "a per-call retry policy wins" $
        withMockApi [json status503 [] ""] $ \port seen -> do
          (client, _) <- mockClient port id
          _ <- sendEither client (withRetryPolicy noRetries billing)
          attempts seen >>= (@?= 1)
    , testCase "stops when the retry budget would be exceeded" $
        withMockApi [json status503 [] ""] $ \port seen -> do
          (client, _) <-
            mockClient port $ \c ->
              c {configRetryPolicy = (configRetryPolicy c) {retryInitialBackoff = 1, retryMaxBackoff = 1, retryBudget = Just 0.5}}
          _ <- sendEither client billing
          attempts seen >>= (@?= 1)
    , testCase "times out a slow attempt" $
        withMockApi [threadDelay 2000000 >> okNoul] $ \port _ -> do
          (client, _) <- mockClient port (\c -> c {configRetryPolicy = noRetries})
          result <- sendEither client (withTimeout 0.2 billing)
          case result of
            Left (ConnectionError (ConnectionTimedOut endpoint)) -> endpoint @?= "POST /v1/systemone"
            other -> assertFailure (show (fmap evaluationAnswers other))
    , testCase "reports a refused connection without leaking the key" $ do
        port <- withMockApi [okNoul] (\port _ -> pure port) -- the port is closed afterwards
        (client, events) <- mockClient port id
        result <- sendEither client billing
        case result of
          Left err@(ConnectionError (ConnectionFailed _ _)) -> do
            let shown = show err <> Text.unpack (renderTypeSafeError err)
            assertBool "the key must not appear in the error" (not ("ts_test_secret_value" `isInfixOf` shown))
          other -> assertFailure (show (fmap evaluationAnswers other))
        stages <- map logStage <$> readIORef events
        length [() | Sending <- stages] @?= 3
    , testCase "rejects invalid questions without sending anything" $
        withMockApi [okNoul] $ \port seen -> do
          (client, _) <- mockClient port id
          result <- sendEither client (systemOne "state" (pure ()))
          case result of
            Left (InvalidRequest NoQuestions) -> pure ()
            other -> assertFailure (show (fmap evaluationAnswers other))
          attempts seen >>= (@?= 0)
    , testCase "send throws what sendEither returns" $
        withMockApi [json status401 [] "{\"detail\":{\"error_type\":\"authentication_error\",\"message\":\"Cannot authenticate\"}}"] $
          \port _ -> do
            (client, _) <- mockClient port id
            thrown <- try (send client billing >>= evaluate)
            case thrown of
              Left (ServiceError e) -> apiErrorKind e @?= Unauthorized
              Left other -> assertFailure (show other)
              Right _ -> assertFailure "expected an exception"
    ]
  where
    mapRetries events = [delay | LogEvent _ _ (Retrying delay _) <- events]
    rightIs result expected = case result of
      Right a -> a @?= expected
      Left e -> assertFailure (show e)

configTests :: TestTree
configTests =
  testGroup
    "configuration"
    [ testCase "API keys are trimmed and validated" $ do
        fmap show (mkApiKey " ts_abc\n") @?= Right "<api key>"
        mkApiKey "" `isLeftWith` "empty"
        mkApiKey "ts abc" `isLeftWith` "whitespace"
        mkApiKey "ts_\233" `isLeftWith` "non-ASCII"
    , -- The environment is global to the process and tasty runs tests in
      -- parallel, so every scenario that changes it lives in this one test.
      testCase "configuration comes from the environment" $ do
        withEnv [("TYPESAFE_API_KEY", Just " ts_env \n"), ("TYPESAFE_BASE_URL", Just "https://gateway.example/typesafe"), ("TYPESAFE_DEFAULT_MODEL", Just "jev-1.13.0")] $ do
          config <- clientConfigFromEnv >>= either (fail . show) pure
          configBaseUrl config @?= "https://gateway.example/typesafe"
          configDefaultModel config @?= "jev-1.13.0"
        withEnv [("TYPESAFE_API_KEY", Just "ts_env"), ("TYPESAFE_BASE_URL", Just "  "), ("TYPESAFE_DEFAULT_MODEL", Nothing)] $ do
          config <- clientConfigFromEnv >>= either (fail . show) pure
          configBaseUrl config @?= defaultBaseUrl
          configDefaultModel config @?= jevLatest
        withEnv [("TYPESAFE_API_KEY", Nothing)] $
          fmap (either Just (const Nothing)) clientConfigFromEnv >>= (@?= Just MissingApiKey)
        withEnv [("TYPESAFE_API_KEY", Just "   ")] $
          fmap (either Just (const Nothing)) clientConfigFromEnv >>= (@?= Just MissingApiKey)
    , testCase "invalid base URLs and timeouts are rejected" $ do
        manager <- newManager defaultManagerSettings
        let rejected config = either Just (const Nothing) (newClientWith manager config)
        fmap isInvalidUrl (rejected (defaultClientConfig testKey) {configBaseUrl = "not a url"}) @?= Just True
        fmap isInvalidUrl (rejected (defaultClientConfig testKey) {configBaseUrl = "https://api.typesafe.ai?x=1"}) @?= Just True
        rejected (defaultClientConfig testKey) {configTimeout = Just 0} @?= Just (InvalidTimeout 0)
    , testCase "newClient throws a ConfigError" $ do
        thrown <- try (newClient (defaultClientConfig testKey) {configBaseUrl = "ftp:/nope"})
        case thrown of
          Left (InvalidBaseUrl url _) -> url @?= "ftp:/nope"
          Left other -> assertFailure (show other)
          Right _ -> assertFailure "expected an exception"
    ]
  where
    isInvalidUrl (InvalidBaseUrl _ _) = True
    isInvalidUrl _ = False
    isLeftWith result fragment = case result of
      Left (InvalidApiKey reason) -> assertBool (Text.unpack reason) (fragment `Text.isInfixOf` reason)
      Left other -> assertFailure (show other)
      Right _ -> assertFailure "expected the key to be rejected"

withEnv :: [(String, Maybe String)] -> IO a -> IO a
withEnv vars action = do
  saved <- traverse (\(k, _) -> (,) k <$> lookupEnv k) vars
  mapM_ set vars
  result <- try action
  mapM_ set saved
  either (\e -> fail (show (e :: SomeException))) pure result
  where
    set (k, v) = maybe (unsetEnv k) (setEnv k) v

-- | Requests against the real API, when enabled. The configuration is read
-- before any test runs, because other tests change the environment.
liveTests :: IO [TestTree]
liveTests = do
  enabled <- lookupEnv "TYPESAFE_LIVE_TESTS"
  configured <- clientConfigFromEnv
  pure $ case configured of
    Right config
      | enabled == Just "1" ->
          [ testGroup
              "live API"
              [ testCase "lists models" $ do
                  client <- newClient config
                  models <- send client listModels
                  assertBool "at least one model" (not (null models))
              , testCase "answers a typed question" $ do
                  client <- newClient config
                  result <- send client billing
                  let p = noulProbability (evaluationAnswers result)
                  assertBool "a probability" (p >= 0 && p <= 1)
                  assertBool "a request id" (isJust (evaluationRequestId result))
              , testCase "rejects a bad key" $ do
                  client <- newClient config {configApiKey = testKey, configRetryPolicy = noRetries}
                  result <- sendEither client listModels
                  case result of
                    Left (ServiceError e) -> unless (apiErrorKind e `elem` [Unauthorized, PermissionDenied]) (assertFailure (show e))
                    other -> assertFailure (show other)
              ]
          ]
    _ -> []
