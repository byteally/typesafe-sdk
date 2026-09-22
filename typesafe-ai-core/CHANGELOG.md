# Changelog for typesafe-ai-core

This package follows the [PVP](https://pvp.haskell.org) and is released
together with `typesafe-ai`, with the same version number.

## 0.1.0.0

First release. Checked against version 0.2.0 of the TypeSafe OpenAPI
specification.

- `TypeSafe.Wire`: every schema of the specification, with JSON codecs.
  Unknown question and answer types are preserved (`QuestionOther`,
  `AnswerOther`).
- `TypeSafe.Question`: typed Noul, Choice and Score questions. Options and
  levels come from `ChoiceOption` and `ScoreLevel` instances, which can be
  derived for enumerations. Questions combine with the `Applicative` instance
  of `Questions`.
- `TypeSafe.Call`: `systemOne`, `systemOneRaw` and `listModels` as values,
  per-call options, and the functions a transport needs.
- `TypeSafe.Error`: `TypeSafeError`, with error responses classified by
  status and error bodies parsed.
- `TypeSafe.Retry`: the retry policy of the official SDKs, as pure functions.
