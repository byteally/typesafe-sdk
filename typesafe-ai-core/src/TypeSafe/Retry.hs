{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TypeSafe.Retry
-- Description : When and how long to wait before retrying
-- Copyright   : (c) 2026 byteally
-- License     : BSD-3-Clause
--
-- Retrying is the transport's job, but the policy is plain data so that
-- every transport behaves the same way. The defaults match the official
-- Python and JavaScript SDKs:
--
-- * up to 2 retries after the first attempt;
-- * retry on HTTP 408, 429 and any 5xx (including TypeSafe's 529
--   "overloaded"), on connection failures and on timeouts;
-- * exponential backoff from 0.5 s, doubling up to 5 s, with up to 25 %
--   jitter;
-- * honour @Retry-After@ and @retry-after-ms@ when the server asks for a
--   delay of at most 60 s;
-- * give up once 30 s have passed since the first attempt.
--
-- Every function here is pure; randomness and time are passed in, which also
-- makes the policy easy to test.
module TypeSafe.Retry
  ( -- * Policy
    RetryPolicy (..)
  , defaultRetryPolicy
  , noRetries

    -- * Decisions
  , isRetryable
  , retryDelay
  , backoffDelay
  , applyJitter
  , retryAfter
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import qualified Data.ByteString.Char8 as BS8
import Data.Char (digitToInt, isDigit)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)
import Network.HTTP.Types (ResponseHeaders, statusCode)
import TypeSafe.Error
  ( ApiError (..)
  , ConnectionError (..)
  , TypeSafeError (..)
  )

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Time (UTCTime (..), fromGregorian)
-- >>> let now = UTCTime (fromGregorian 2026 9 22) 0

-- | How a transport retries failed requests. Durations are in seconds.
data RetryPolicy = RetryPolicy
  { retryMaxRetries :: !Int
  -- ^ Retries after the first attempt. @0@ disables retrying.
  , retryInitialBackoff :: !NominalDiffTime
  -- ^ The first backoff delay. It doubles with every retry.
  , retryMaxBackoff :: !NominalDiffTime
  -- ^ The longest backoff delay.
  , retryJitter :: !Double
  -- ^ The largest fraction, between 0 and 1, randomly taken off each backoff
  -- delay so that clients do not retry in lockstep.
  , retryStatuses :: !(Set Int)
  -- ^ The HTTP status codes worth retrying.
  , retryRespectRetryAfter :: !Bool
  -- ^ Wait as long as the server asks with @Retry-After@ or
  -- @retry-after-ms@, up to 'retryMaxRetryAfter'.
  , retryMaxRetryAfter :: !NominalDiffTime
  -- ^ The longest server-requested delay to honour. Longer requests fall back
  -- to the backoff delay.
  , retryConnectionErrors :: !Bool
  -- ^ Retry requests that failed without a response.
  , retryTimeouts :: !Bool
  -- ^ Retry requests that timed out.
  , retryBudget :: !(Maybe NominalDiffTime)
  -- ^ Stop retrying when the next attempt would start this long after the
  -- first one. 'Nothing' means no limit.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The policy described in the module header.
defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy =
  RetryPolicy
    { retryMaxRetries = 2
    , retryInitialBackoff = 0.5
    , retryMaxBackoff = 5
    , retryJitter = 0.25
    , retryStatuses = Set.fromList (408 : 429 : [500 .. 599])
    , retryRespectRetryAfter = True
    , retryMaxRetryAfter = 60
    , retryConnectionErrors = True
    , retryTimeouts = True
    , retryBudget = Just 30
    }

-- | Never retry.
noRetries :: RetryPolicy
noRetries = defaultRetryPolicy {retryMaxRetries = 0}

-- | Whether the policy retries this error (ignoring the retry count and
-- budget). Local validation errors and unusable responses are never retried:
-- sending the same request again would fail the same way.
isRetryable :: RetryPolicy -> TypeSafeError -> Bool
isRetryable policy = \case
  ServiceError e -> statusCode (apiErrorStatus e) `Set.member` retryStatuses policy
  ConnectionError (ConnectionTimedOut _) -> retryTimeouts policy
  ConnectionError (ConnectionFailed _ _) -> retryConnectionErrors policy
  InvalidRequest _ -> False
  ResponseError _ -> False

-- | How long to wait before retry number @n@ (counting from 1).
--
-- A delay requested by the server wins when the policy honours it and it is
-- short enough; otherwise it is the backoff delay with jitter applied. The
-- 'Double' is a uniformly random number in [0, 1) that drives the jitter.
--
-- >>> retryDelay defaultRetryPolicy now 0 1 (ConnectionError (ConnectionTimedOut "POST /v1/systemone"))
-- 0.5s
retryDelay :: RetryPolicy -> UTCTime -> Double -> Int -> TypeSafeError -> NominalDiffTime
retryDelay policy now random n err = case serverDelay of
  Just d | retryRespectRetryAfter policy && d <= retryMaxRetryAfter policy -> d
  _ -> applyJitter policy random (backoffDelay policy n)
  where
    serverDelay = case err of
      ServiceError e -> retryAfter now (apiErrorHeaders e)
      _ -> Nothing

-- | The backoff delay before retry number @n@ (counting from 1), without
-- jitter.
--
-- >>> map (backoffDelay defaultRetryPolicy) [1 .. 6]
-- [0.5s,1s,2s,4s,5s,5s]
backoffDelay :: RetryPolicy -> Int -> NominalDiffTime
backoffDelay policy n =
  min (retryMaxBackoff policy) (retryInitialBackoff policy * 2 ^ max 0 (n - 1))

-- | Take a random part of at most 'retryJitter' off a delay. The 'Double' is
-- a uniformly random number in [0, 1).
--
-- >>> applyJitter defaultRetryPolicy 0.5 4
-- 3.5s
applyJitter :: RetryPolicy -> Double -> NominalDiffTime -> NominalDiffTime
applyJitter policy random delay =
  delay * realToFrac (1 - clamp (retryJitter policy) * clamp random)
  where
    clamp = max 0 . min 1

-- | The delay the server asks for, if any, from the @retry-after-ms@ header
-- (milliseconds) or the standard @Retry-After@ header (seconds, or an HTTP
-- date relative to the given current time).
--
-- >>> retryAfter now [("retry-after-ms", "250")]
-- Just 0.25s
--
-- >>> retryAfter now [("Retry-After", "3")]
-- Just 3s
--
-- >>> retryAfter now [("Retry-After", "Tue, 22 Sep 2026 00:00:10 GMT")]
-- Just 10s
retryAfter :: UTCTime -> ResponseHeaders -> Maybe NominalDiffTime
retryAfter now headers =
  case lookup "retry-after-ms" headers >>= decimal of
    Just ms -> Just (fromRational (ms / 1000))
    Nothing -> lookup "retry-after" headers >>= \v -> (fromRational <$> decimal v) <|> date v
  where
    date v =
      max 0 . (`diffUTCTime` now)
        <$> parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" (BS8.unpack (BS8.strip v))

-- | A non-negative decimal number such as @3@ or @1.25@, parsed exactly.
decimal :: BS8.ByteString -> Maybe Rational
decimal raw = case BS8.split '.' (BS8.strip raw) of
  [whole] | digits whole -> Just (integer whole)
  [whole, fraction]
    | digits whole && digits fraction ->
        Just (integer whole + integer fraction / 10 ^ BS8.length fraction)
  _ -> Nothing
  where
    digits s = not (BS8.null s) && BS8.all isDigit s
    integer = fromInteger . BS8.foldl' (\acc c -> acc * 10 + toInteger (digitToInt c)) 0
