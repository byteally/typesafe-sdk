{-# LANGUAGE OverloadedStrings #-}

-- | List the models available to the account, then pin a model version for
-- one call and handle errors explicitly.
--
-- > TYPESAFE_API_KEY=... cabal run typesafe-models
module Main (main) where

import Control.Exception (throwIO)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import TypeSafe

main :: IO ()
main = do
  config <- clientConfigFromEnv >>= either throwIO pure
  client <- newClient config {configLogger = stderrLogger}

  models <- send client listModels
  mapM_
    (\m -> putStrLn (show (modelMetadataName m) <> "  " <> show (modelMetadataReleaseDay m) <> "  " <> Text.unpack (modelMetadataDescription m)))
    models

  -- Pinning a version keeps answers stable when the alias moves.
  outcome <-
    sendEither client . withModel "jev-1.13.0" . withTimeout 5 $
      systemOne "The invoice total is wrong." (ask "billing" (noul "Is this about billing?"))
  case outcome of
    Right result -> putStrLn ("billing: " <> show (noulProbability (evaluationAnswers result)))
    Left (ServiceError e)
      | apiErrorKind e `elem` [BadRequest, UnprocessableEntity] ->
          Text.putStrLn ("The request was rejected: " <> fromMaybe "no details" (apiErrorMessage e))
    Left err -> Text.putStrLn (renderTypeSafeError err)
