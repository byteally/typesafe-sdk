module Main (main) where

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Test.Tasty (defaultMain, testGroup)
import qualified TypeSafe.ConformanceSpec as Conformance
import qualified TypeSafe.ErrorSpec as Error
import TypeSafe.JsonSchema (loadSpec)
import qualified TypeSafe.QuestionSpec as Question
import qualified TypeSafe.RetrySpec as Retry

-- | Set @TYPESAFE_OPENAPI_SPEC@ to check the bindings against another copy of
-- the specification, such as a freshly downloaded one, without replacing the
-- vendored file.
main :: IO ()
main = do
  specPath <- fromMaybe "spec/openapi.json" <$> lookupEnv "TYPESAFE_OPENAPI_SPEC"
  spec <- loadSpec specPath
  defaultMain $
    testGroup
      "typesafe-ai-core"
      [ Conformance.tests spec
      , Question.tests
      , Error.tests
      , Retry.tests
      ]
