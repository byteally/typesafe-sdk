# typesafe-sdk

[![CI](https://github.com/byteally/typesafe-sdk/actions/workflows/ci.yml/badge.svg)](https://github.com/byteally/typesafe-sdk/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/typesafe-ai.svg)](https://hackage.haskell.org/package/typesafe-ai)

Haskell SDK for [TypeSafe AI](https://typesafe.ai)'s System One API. Send
text or JSON state with typed questions, and get back answers that decode to
your own Haskell types:

```haskell
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies, OverloadedStrings #-}

import GHC.Generics (Generic)
import TypeSafe

data Department = Billing | Technical | Sales
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ChoiceOption)

data Frustration = Calm | Frustrated | VeryAngry
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ScoreLevel)

data Triage = Triage
  { department :: Choice Department
  , urgent :: Noul
  , frustration :: Score Frustration
  }

triage :: Questions Triage
triage =
  Triage
    <$> ask "department" (choice "Which team should handle this?")
    <*> ask "is_urgent" (noul "Does this convey urgency?")
    <*> ask "frustration" (score "How frustrated is the customer?")

main :: IO ()
main = do
  client <- newClientFromEnv -- reads TYPESAFE_API_KEY
  result <- send client (systemOne "Help! My payouts have been failing for 3 days." triage)
  let answers = evaluationAnswers result
  print (choiceSelected (department answers), choiceConfidence (department answers))
  print (noulProbability (urgent answers))
  print (mostLikelyLevel (frustration answers))
```

`choiceSelected` is a `Department`: an option the model was not offered can
never reach your code. All three questions go out in one request and are
answered in parallel.

## Packages

| Package | What it contains | Depends on |
| --- | --- | --- |
| [`typesafe-ai`](typesafe-ai) | The client: `newClientFromEnv`, `send`, retries, timeouts, logging hooks. Re-exports everything below. | `http-client`, `http-client-tls` |
| [`typesafe-ai-core`](typesafe-ai-core) | Typed questions and answers, the OpenAPI schemas as Haskell types with JSON codecs, API calls as values, errors, the retry policy. **No HTTP library.** | `aeson`, `http-types`, boot packages |

Most applications depend on `typesafe-ai` only. Depend on `typesafe-ai-core`
alone to use your own HTTP stack, such as servant.

## Installing

Add the package to your `.cabal` file:

```cabal
build-depends: typesafe-ai ^>=0.1
```

The SDK supports GHC 9.6 to 9.14. CI builds and tests it on Linux, and with
one GHC version on macOS and Windows.

## Documentation

- [`TypeSafe.Tutorial`](https://hackage.haskell.org/package/typesafe-ai/docs/TypeSafe-Tutorial.html):
  a guided tour, from the first request to confidence routing, composite
  scores, error handling and testing.
- The module documentation on Hackage. Every example that can run without a
  network is checked by [doctest](https://github.com/sol/doctest).
- [`examples/`](examples): runnable programs. Try
  `TYPESAFE_API_KEY=... cabal run typesafe-quickstart`.
- TypeSafe's own documentation: <https://docs.typesafe.ai>.

## Design

### Types all the way to the answer

- **Options and levels are types.** `choice` and `score` take their options
  and levels from a type with a `ChoiceOption` or `ScoreLevel` instance,
  usually derived for an enumeration. The answer is decoded back into that
  type, and an answer outside it is reported as an error.
- **Questions compose with `Applicative`.** `Questions a` describes a request
  and how to build an `a` from its answers. It is deliberately not a `Monad`:
  questions in one request are answered independently, so a question that
  depends on another's answer needs a second request, and the types say so.
- **Probability and confidence are distinct types.** `Probability` and
  `Confidence` are newtypes, so a Noul's probability cannot be mistaken for a
  Choice's confidence. Both still accept numeric literals (`>= 0.8`).
- **Errors are values.** `TypeSafeError` separates requests rejected locally,
  error responses (classified as `Unauthorized`, `RateLimited`,
  `Overloaded`, …), connection failures and responses that do not fit the
  request. `sendEither` returns them; `send` throws them.

### Independent of any web framework

`typesafe-ai-core` does no I/O. An API call is a value, `Call a`, that a
transport renders to an `HttpRequest`, sends however it likes, and decodes
from an `HttpResponse`. The bundled transport uses `http-client`. The retry
policy, error classification and header handling live in the core, so
every transport behaves the same way.

To use servant or anything else, you can either describe the API
over the wire types:

```haskell
import qualified TypeSafe.Wire as Wire

type TypeSafeAPI =
  Header' '[Required, Strict] "Authorization" Text
    :> "v1"
    :> ( "systemone" :> ReqBody '[JSON] Wire.SystemOneRequest :> Post '[JSON] Wire.SystemOneResponse
           :<|> "models" :> Get '[JSON] Wire.ModelMetadataList
       )
```

and still use typed questions with `systemOneRequest` and
`decodeEvaluation`, or write a transport around `renderCall` and
`parseResponse` to reuse the retry and error logic as well.

### Why `http-client`

The client is built on `http-client` and `http-client-tls`, the foundation
that servant-client and most other Haskell HTTP libraries are built on. It is
small, stable and maintained, and gives direct control over what an SDK
needs: a shared, thread-safe connection `Manager`, per-request timeouts,
streaming bodies and exception types. Applications that already use
servant-client or `http-conduit` already have it, and can pass their
`Manager` to `newClientWith`.

The SDK talks to two endpoints with JSON bodies, so the layer over
`http-client` is small. Because the transport is separate from everything
else, switching to another HTTP library later would change only
`TypeSafe.Client`.

### Efficiency

- Requests are encoded straight to bytes with aeson's `toEncoding`, without
  building an intermediate `Value`. Choice options keep the order you
  declared them in.
- One `Client` keeps a pool of keep-alive TLS connections and is safe to
  share between threads (see [`examples/Concurrent.hs`](examples/Concurrent.hs)).
- Many questions fit in one request, and the docs encourage it: extra
  questions barely change latency and cost only their own tokens.

## Keeping up with the API

TypeSafe publishes an OpenAPI specification at
<https://api.typesafe.ai/openapi.json>. A copy is vendored in
[`typesafe-ai-core/spec/openapi.json`](typesafe-ai-core/spec/openapi.json),
and the bindings are tested against it:

- **Conformance tests** check that every operation and schema in the
  specification is bound. They also check that every property is encoded,
  that required properties are required and optional ones optional, and that
  the discriminators match the Haskell constructors. Generated values must
  validate against the schemas, and the specification's own examples must
  decode. The tests fail when the specification uses a JSON Schema keyword
  they do not understand, rather than silently passing.
- **A daily workflow** ([`spec-drift.yml`](.github/workflows/spec-drift.yml))
  fetches the published specification. When it changes, the workflow opens a
  pull request that updates the vendored copy. The PR description summarises
  what changed and lists the conformance tests that now fail: the to-do list
  for the update.
- **`scripts/sync-spec.sh`** does the same locally.

The SDK keeps working with newer APIs in the meantime. It ignores unknown
response fields and keeps answers of unknown types. `withExtraBody` sends new
request fields, and `otherQuestion` sends new question types.

[MAINTAINING.md](MAINTAINING.md) describes how to update the bindings and
release them.

### Versions

Both packages follow the [PVP](https://pvp.haskell.org) and are released
together, with the same version number. `apiSpecVersion` reports the API
specification a release was checked against.

| SDK | TypeSafe API specification |
| --- | --- |
| 0.1.0.0 | 0.2.0 |

## Development

```sh
cabal build all
cabal test all                    # unit, conformance and mock-server tests
TYPESAFE_LIVE_TESTS=1 TYPESAFE_API_KEY=... cabal test typesafe-ai   # plus the live API
scripts/sync-spec.sh --check      # is the vendored specification current?
```

Run the documentation examples with
[doctest](https://github.com/sol/doctest) (needs cabal 3.16 or later for
`--with-repl`):

```sh
cabal install doctest --ignore-project
cabal repl typesafe-ai-core --with-repl=doctest --repl-options=-w
cabal repl typesafe-ai --with-repl=doctest --repl-options=-w
```

## License

BSD-3-Clause. See [LICENSE](LICENSE).

This is a community SDK, maintained by [byteally](https://github.com/byteally).
It is not affiliated with or endorsed by TypeSafe AI.
