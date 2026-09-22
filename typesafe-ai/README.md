# typesafe-ai

A Haskell client for [TypeSafe AI](https://typesafe.ai)'s System One API.
Send text or JSON state with typed questions, and get back answers that decode
to your own Haskell types.

```haskell
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies, OverloadedStrings #-}

import GHC.Generics (Generic)
import TypeSafe

data Department = Billing | Technical | Sales
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ChoiceOption)

main :: IO ()
main = do
  client <- newClientFromEnv -- reads TYPESAFE_API_KEY
  result <-
    send client $
      systemOne "Help! My payouts have been failing for 3 days." $
        (,) <$> ask "department" (choice "Which team should handle this?")
            <*> ask "is_urgent" (noul "Does this convey urgency?")
  let (department, urgent) = evaluationAnswers result
  print (choiceSelected department :: Department, choiceConfidence department)
  print (noulProbability urgent)
```

- Choice options and Score levels are your own types; the answer can only be
  one of them.
- Questions combine with `Applicative`, and many questions fit in one
  request.
- Built on `http-client`, with a shared TLS connection pool, per-attempt
  timeouts, and retries with exponential backoff that honour `Retry-After`.
- Errors are values that separate local validation, error responses,
  connection failures and unexpected responses.

Read `TypeSafe.Tutorial` for a guided tour. The types, codecs and calls live
in [`typesafe-ai-core`](https://hackage.haskell.org/package/typesafe-ai-core),
which has no HTTP dependency. The
[repository](https://github.com/byteally/typesafe-sdk) has runnable examples.

This is a community SDK, not affiliated with or endorsed by TypeSafe AI.
