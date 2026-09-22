# typesafe-ai-core

Transport-agnostic bindings to [TypeSafe AI](https://typesafe.ai)'s System
One API: typed Noul, Choice and Score questions whose answers decode to your
own Haskell types, a one-to-one mirror of the OpenAPI schemas with JSON
codecs, and API calls as plain values.

This package does no networking. Use it with servant or any other HTTP
stack. For a ready-to-use client, depend on
[`typesafe-ai`](https://hackage.haskell.org/package/typesafe-ai) instead; it
re-exports everything here.

```haskell
data Department = Billing | Technical | Sales
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ChoiceOption)

routing :: Questions (Choice Department, Noul)
routing =
  (,) <$> ask "department" (choice "Which team should handle this?")
      <*> ask "is_urgent" (noul "Does this convey urgency?")

-- The request, for any HTTP client:
request :: Either RequestError Wire.SystemOneRequest
request = systemOneRequest jevLatest "Help! My payouts have been failing." routing

-- And the typed answers, from its response:
answers :: Wire.SystemOneResponse -> Either (NonEmpty AnswerError) (Evaluation (Choice Department, Noul))
answers = decodeEvaluation routing Nothing
```

Start with the documentation of `TypeSafe.Core` and `TypeSafe.Question`. The
[repository](https://github.com/byteally/typesafe-sdk) has more examples.

The bindings are checked against version `0.2.0` of the TypeSafe OpenAPI
specification (`apiSpecVersion`), which is included in the package
(`spec/openapi.json`).

This is a community SDK, not affiliated with or endorsed by TypeSafe AI.
