{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Confidence-gated routing (<https://docs.typesafe.ai/patterns/confidence-routing>):
-- act on the answer automatically when the model is confident, and send the
-- ranked alternatives to a person otherwise.
--
-- > TYPESAFE_API_KEY=... cabal run typesafe-confidence-routing
module Main (main) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import TypeSafe

data Intent = Refund | Rebooking | Information | Other
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ChoiceOption)

data Route
  = Automate Intent
  | Review (NonEmpty (Intent, Probability))
  deriving stock (Show)

route :: Choice Intent -> Route
route answer
  | confidence answer >= 0.8 = Automate (choiceSelected answer)
  | otherwise = Review (rankedChoices answer)

main :: IO ()
main = do
  client <- newClientFromEnv
  let messages :: [Text]
      messages =
        [ "My flight was cancelled. I want my money back."
        , "Can I change my booking, or get a voucher, or maybe a refund? Not sure what's best."
        ]
  mapM_
    ( \message -> do
        result <- send client (systemOne (contentText message) (ask "intent" (choice "What is the main request?")))
        putStrLn (Text.unpack message)
        putStrLn ("  -> " <> show (route (evaluationAnswers result)))
    )
    messages
