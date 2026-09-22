-- |
-- Module      : TypeSafe
-- Description : Typed questions for the TypeSafe System One API
-- Copyright   : (c) 2026 byteally
-- License     : BSD-3-Clause
--
-- A client for TypeSafe's System One API (<https://docs.typesafe.ai>).
-- Send a /state/ (text or JSON) with typed questions, and get back answers
-- your code can use directly: a probability for a yes\/no 'noul', one of your
-- own options for a 'choice', a level of your own rubric for a 'score'.
--
-- @
-- {-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies, OverloadedStrings #-}
--
-- import GHC.Generics (Generic)
-- import TypeSafe
--
-- data Department = Billing | Technical | Sales
--   deriving stock (Show, Generic)
--   deriving anyclass ('ChoiceOption')
--
-- main :: IO ()
-- main = do
--   client <- 'newClientFromEnv'
--   result <-
--     'send' client $
--       'systemOne' \"Help! My payouts have been failing for 3 days.\" $
--         (,)
--           \<$\> 'ask' \"department\" ('choice' \"Which team should handle this?\")
--           \<*\> 'ask' \"is_urgent\" ('noul' \"Does this convey urgency?\")
--   let (department, urgent) = 'evaluationAnswers' result
--   print ('choiceSelected' department :: Department, 'choiceConfidence' department)
--   print ('noulProbability' urgent)
-- @
--
-- "TypeSafe.Tutorial" walks through the library step by step. The other
-- modules are:
--
-- ["TypeSafe.Question"] questions and answers ('noul', 'choice', 'score',
-- 'ask').
--
-- ["TypeSafe.Call"] the API calls ('systemOne', 'listModels') and per-call
-- options ('withModel', 'withTimeout', …).
--
-- ["TypeSafe.Client"] the HTTP client ('newClientFromEnv', 'send').
--
-- ["TypeSafe.Error"], ["TypeSafe.Retry"] errors and the retry policy.
--
-- ["TypeSafe.Wire"] the raw OpenAPI schemas. Not re-exported.
--
-- The types, codecs and calls live in the @typesafe-ai-core@ package, which
-- has no HTTP dependency; this package adds the @http-client@ transport.
module TypeSafe
  ( module TypeSafe.Core
  , module TypeSafe.Client
  ) where

import TypeSafe.Client
import TypeSafe.Core
