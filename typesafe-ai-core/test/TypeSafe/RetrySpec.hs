{-# LANGUAGE OverloadedStrings #-}

module TypeSafe.RetrySpec (tests) where

import Control.Exception (toException)
import Data.Time (NominalDiffTime, UTCTime (..), addUTCTime, fromGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Types (ResponseHeaders, mkStatus)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (choose, forAll, testProperty, (.&&.))
import TypeSafe.Error
import TypeSafe.Retry

now :: UTCTime
now = UTCTime (fromGregorian 2026 9 22) 43200

serviceError :: Int -> ResponseHeaders -> TypeSafeError
serviceError c headers = ServiceError (apiErrorFromResponse "POST /v1/systemone" (mkStatus c "") headers "")

tests :: TestTree
tests =
  testGroup
    "Retries"
    [ testCase "transient statuses are retried, client errors are not" $
        map (\c -> isRetryable defaultRetryPolicy (serviceError c [])) [408, 429, 500, 503, 529, 400, 401, 403, 404, 422]
          @?= [True, True, True, True, True, False, False, False, False, False]
    , testCase "connection failures and timeouts follow their switches" $ do
        let failed = ConnectionError (ConnectionFailed "GET /v1/models" (toException (userError "reset")))
            timedOut = ConnectionError (ConnectionTimedOut "GET /v1/models")
        map (isRetryable defaultRetryPolicy) [failed, timedOut] @?= [True, True]
        isRetryable defaultRetryPolicy {retryConnectionErrors = False} failed @?= False
        isRetryable defaultRetryPolicy {retryTimeouts = False} timedOut @?= False
    , testCase "invalid requests are never retried" $
        isRetryable defaultRetryPolicy (InvalidRequest NoQuestions) @?= False
    , testCase "backoff doubles up to the maximum" $
        map (backoffDelay defaultRetryPolicy) [1 .. 6] @?= [0.5, 1, 2, 4, 5, 5]
    , testCase "zero initial backoff disables waiting" $
        backoffDelay defaultRetryPolicy {retryInitialBackoff = 0} 3 @?= 0
    , testProperty "jitter only shortens the delay, by at most the jitter fraction" $
        forAll (choose (0, 0.999999)) $ \u ->
          let d = applyJitter defaultRetryPolicy u 4
           in (d <= 4) .&&. (d >= 3)
    , testCase "retry-after-ms wins over Retry-After" $
        retryAfter now [("Retry-After", "10"), ("retry-after-ms", "1500")] @?= Just 1.5
    , testCase "Retry-After as seconds and as a date" $ do
        retryAfter now [("retry-after", "7")] @?= Just 7
        retryAfter now [("Retry-After", httpDate (addUTCTime 42 now))] @?= Just 42
        retryAfter now [("Retry-After", httpDate (addUTCTime (-5) now))] @?= Just 0
    , testCase "malformed Retry-After headers are ignored" $ do
        retryAfter now [("Retry-After", "soon")] @?= Nothing
        retryAfter now [("Retry-After", "-3")] @?= Nothing
        retryAfter now [] @?= Nothing
    , testCase "a short server delay replaces the backoff" $
        retryDelay defaultRetryPolicy now 0.5 1 (serviceError 429 [("retry-after-ms", "200")]) @?= 0.2
    , testCase "a server delay above the maximum falls back to backoff" $
        retryDelay defaultRetryPolicy now 0 2 (serviceError 429 [("Retry-After", "3600")]) @?= 1
    , testCase "server delays can be ignored" $
        retryDelay defaultRetryPolicy {retryRespectRetryAfter = False} now 0 1 (serviceError 429 [("Retry-After", "3")])
          @?= (0.5 :: NominalDiffTime)
    ]
  where
    httpDate = BS8.pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"
