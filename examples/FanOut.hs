{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Two patterns from <https://docs.typesafe.ai/patterns>, each in a single
-- request:
--
-- * /speculative fan-out/ with /composite scoring/: ask every question the
--   code might need about a bug report, weight the scores in code, and use
--   the severity only if it really is a bug report;
--
-- * /re-ranking/: one question per candidate passage, generated with
--   'traverse'.
--
-- > TYPESAFE_API_KEY=... cabal run typesafe-fan-out
module Main (main) where

import Data.Aeson ((.=))
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import GHC.Generics (Generic)
import TypeSafe

data Severity = Cosmetic | Degraded | Outage
  deriving stock (Show, Eq, Generic)

instance ScoreLevel Severity where
  levelDescription = \case
    Cosmetic -> "Cosmetic: nothing is broken"
    Degraded -> "Degraded: a feature misbehaves but there is a workaround"
    Outage -> "Outage: customers cannot use the product"

data Detail = Vague | Partial | Reproducible
  deriving stock (Show, Eq, Generic)

instance ScoreLevel Detail where
  levelDescription = \case
    Vague -> "Vague: no steps, versions or errors"
    Partial -> "Partial: some context, but an engineer would need to ask questions"
    Reproducible -> "Reproducible: steps, versions and error messages are included"

data Report = Report
  { isBugReport :: Noul
  , severity :: Score Severity
  , detail :: Score Detail
  , frustration :: Score Int
  }

-- | Every question is asked, even the ones that only matter for bug reports:
-- extra questions in the same request are nearly free.
report :: Questions Report
report =
  Report
    <$> ask "is_bug_report" (noul "Does the message report a software defect?")
    <*> ask "severity" (score "How severe is the reported problem?")
    <*> ask "detail" (score "How much does the report give an engineer to work with?")
    <*> ask "frustration" (scoreRubric "How frustrated is the customer?" ("Calm" :| ["Annoyed", "Angry"]))

-- | Weights live in code, where they are easy to review and change.
priority :: Report -> Maybe Double
priority r
  | noulProbability (isBugReport r) < 0.5 = Nothing
  | otherwise =
      Just $
        0.5 * normalizedScore (severity r)
          + 0.3 * normalizedScore (frustration r)
          + 0.2 * normalizedScore (detail r)

-- | One Noul per passage, all in the same request.
relevance :: Text -> [Text] -> Questions [(Text, Noul)]
relevance query passages =
  for (zip [0 :: Int ..] passages) $ \(i, passage) ->
    (,) passage
      <$> ask
        (QuestionId ("passage_" <> Text.pack (show i)))
        ( noul
            ( contentObject
                [ "query" .= query
                , "passage" .= passage
                , "question" .= ("Does `passage` answer `query`?" :: Text)
                ]
            )
        )

main :: IO ()
main = do
  client <- newClientFromEnv

  bug <-
    send client . systemOne "Checkout returns HTTP 500 for every order since the 2.3 release. Steps: add any item, press Pay." $
      report
  putStrLn ("Priority: " <> maybe "not a bug report" show (priority (evaluationAnswers bug)))

  let passages =
        [ "Refunds are issued to the original payment method within 5 business days."
        , "Our offices are closed on public holidays."
        , "Duplicate charges are refunded automatically once confirmed by support."
        ]
  ranked <- send client (systemOne "Ranking passages" (relevance "How long does a refund take?" passages))
  mapM_ (\(passage, answer) -> putStrLn (show (noulProbability answer) <> "  " <> Text.unpack passage)) $
    sortOn (Down . noulProbability . snd) (evaluationAnswers ranked)
