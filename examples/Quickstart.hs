{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Triage a support message with one request and three typed questions.
--
-- > TYPESAFE_API_KEY=... cabal run typesafe-quickstart
module Main (main) where

import GHC.Generics (Generic)
import TypeSafe

-- | Which team should handle the message. Deriving 'ChoiceOption' offers the
-- constructors as options; the instance adds descriptions.
data Department = Billing | Technical | Sales
  deriving stock (Show, Eq, Generic)

instance ChoiceOption Department where
  optionDescription =
    Just . \case
      Billing -> "Payments, invoicing, refunds"
      Technical -> "Bugs, outages, integrations"
      Sales -> "Pricing, upgrades, new accounts"

-- | How frustrated the customer is, lowest level first.
data Frustration = Calm | Frustrated | VeryAngry
  deriving stock (Show, Eq, Generic)

instance ScoreLevel Frustration where
  levelDescription = \case
    Calm -> "Calm, just stating facts"
    Frustrated -> "Frustrated but civil"
    VeryAngry -> "Very angry, strong language"

data Triage = Triage
  { department :: Choice Department
  , urgent :: Noul
  , frustration :: Score Frustration
  }

-- | Three independent questions about the same state, sent in one request.
triage :: Questions Triage
triage =
  Triage
    <$> ask "department" (choice "Which team should handle this?")
    <*> ask "is_urgent" (noulWith "Does this convey urgency?" "Explicitly time-sensitive" "No urgency expressed")
    <*> ask "frustration" (score "How frustrated is the customer?")

main :: IO ()
main = do
  client <- newClientFromEnv
  result <- send client (systemOne "Help! My payouts have been failing for 3 days." triage)

  let answers = evaluationAnswers result
  putStrLn $
    "Department:  "
      <> show (choiceSelected (department answers))
      <> " (confidence "
      <> show (choiceConfidence (department answers))
      <> ")"
  putStrLn $ "Urgent:      " <> show (noulProbability (urgent answers))
  putStrLn $
    "Frustration: "
      <> show (mostLikelyLevel (frustration answers))
      <> " (expected level "
      <> show (scoreValue (frustration answers))
      <> ")"
  putStrLn $
    "Answered by "
      <> show (evaluationModel result)
      <> " using "
      <> show (usageInputTokens (evaluationUsage result))
      <> " input tokens"
