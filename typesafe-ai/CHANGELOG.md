# Changelog for typesafe-ai

This package follows the [PVP](https://pvp.haskell.org) and is released
together with `typesafe-ai-core`, with the same version number.

## 0.1.0.0

First release. Checked against version 0.2.0 of the TypeSafe OpenAPI
specification.

- `TypeSafe.Client`: an `http-client` transport with a shared TLS connection
  pool, per-attempt timeouts, retries with exponential backoff and jitter that
  honour `Retry-After` and `retry-after-ms`, and a logging hook.
- Configuration from `TYPESAFE_API_KEY`, `TYPESAFE_BASE_URL` and
  `TYPESAFE_DEFAULT_MODEL`, like the official Python and JavaScript SDKs.
- `TypeSafe` re-exports the whole API, and the package re-exports the modules
  of `typesafe-ai-core`.
- `TypeSafe.Tutorial`: a guided tour of the SDK.
