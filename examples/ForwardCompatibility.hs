{-# LANGUAGE OverloadedStrings #-}

-- | Escape hatches for API features this version of the SDK does not know
-- about yet, and the wire-level API underneath the typed one.
--
-- > TYPESAFE_API_KEY=... cabal run typesafe-forward-compatibility
module Main (main) where

import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import TypeSafe
import qualified TypeSafe.Wire as Wire

main :: IO ()
main = do
  client <- newClientFromEnv

  -- 1. The wire-level request: exactly the JSON the API documents.
  let raw =
        Wire.SystemOneRequest
          { Wire.systemOneRequestState = "I was charged twice. Please help."
          , Wire.systemOneRequestModel = jevLatest
          , Wire.systemOneRequestQuestions =
              Map.fromList
                [ ("billing", Wire.QuestionNoul (Wire.NoulQuestion (Just "Is this about billing?") Nothing))
                ]
          }
  answers <- send client (systemOneRaw raw)
  print (evaluationAnswers answers)

  -- 2. The complete response of a typed call is always available, including
  -- answers of types this SDK version cannot decode.
  typed <- send client (systemOne "I was charged twice." (ask "billing" (noul "Is this about billing?")))
  print (Wire.systemOneResponseAnswers (evaluationResponse typed))

  -- 3. Extra top-level request fields, for request options the SDK does not
  -- know yet. The field below is made up, so the request is only rendered
  -- here, not sent: only send fields the API supports.
  let call =
        withExtraBody (KeyMap.fromList [("example_option", "value")]) $
          systemOne "I was charged twice." (ask "billing" (noul "Is this about billing?"))
  either print (mapM_ LBS.putStrLn . httpRequestBody) (renderCall (CallDefaults jevLatest []) call)
