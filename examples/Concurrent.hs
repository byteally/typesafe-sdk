{-# LANGUAGE OverloadedStrings #-}

-- | Share one 'Client' between threads. The client is thread-safe and reuses
-- connections from its pool, so concurrent requests do not pay for a new TLS
-- handshake each time. Rate limits (HTTP 429) are retried with backoff.
--
-- Prefer putting many questions in one request when they share a state; use
-- concurrency for requests with different states.
--
-- > TYPESAFE_API_KEY=... cabal run typesafe-concurrent
module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import TypeSafe

main :: IO ()
main = do
  client <- newClientFromEnv
  let reviews =
        [ "Arrived broken and support never answered."
        , "Does exactly what it says. Five stars."
        , "It's fine, I guess."
        , "Stopped working after a week, but the replacement is great."
        ]
      positive review = systemOne review (ask "positive" (noul "Is the review positive overall?"))
  results <- mapConcurrently (send client . positive) reviews
  mapM_ (\(review, r) -> putStrLn (show (noulProbability (evaluationAnswers r)) <> "  " <> show review)) (zip reviews results)
