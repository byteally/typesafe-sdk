# Maintaining typesafe-sdk

How to keep the bindings in step with the TypeSafe API and release them to
Hackage.

## Repository layout

| Path | Contents |
| --- | --- |
| `typesafe-ai-core/` | Hackage package: types, codecs, calls. No HTTP. |
| `typesafe-ai-core/spec/openapi.json` | The vendored OpenAPI specification, normalised with `jq -S`. |
| `typesafe-ai-core/test/TypeSafe/ConformanceSpec.hs` | The tests that check the bindings against the specification. |
| `typesafe-ai/` | Hackage package: the `http-client` transport. |
| `examples/` | Example programs. Built by CI, not published. |
| `scripts/sync-spec.sh` | Compares the published specification with the vendored copy. |
| `.github/workflows/` | `ci.yml`, `spec-drift.yml` (daily specification check), `release.yml`. |

## Versioning

Both packages follow the [PVP](https://pvp.haskell.org) (`A.B.C.D`) and are
always released together with the same version. `typesafe-ai` depends on
`typesafe-ai-core >=A.B.C && <A.B.(C+1)` because it re-exports the core's
API.

| Change | Bump |
| --- | --- |
| A field, constructor or type is added to or removed from an exported type; a function changes type; behaviour changes incompatibly | `A.B` |
| New functions, modules or instances only | `C` |
| Bug fixes and documentation | `D` |

Adding a field to a record with an exported constructor breaks code that
builds the record positionally or with record syntax, so under the PVP most
API features count as `A.B` changes. That is expected. The escape hatches
(`withExtraBody`, `otherQuestion`, `AnswerOther`, `evaluationResponse`) let
users adopt a feature before the release.

Every release records the specification version it was checked against:
`apiSpecVersion` in `TypeSafe.Wire`, the changelogs, and the table in
`README.md`.

## When the API changes

### 1. Notice

The `spec-drift` workflow runs every day. When
`https://api.typesafe.ai/openapi.json` differs from the vendored copy, it
opens or updates the `api-spec-update` pull request. The description lists
what was added and removed, and the conformance tests that fail against the
new specification.

The workflow opens the pull request with a GitHub App token, so that CI runs
on it and a bot account is its author. Pull requests opened with the default
`GITHUB_TOKEN` do not trigger other workflows. Without the app, the workflow
still runs, but falls back to `GITHUB_TOKEN`, and CI has to be started by
hand (`gh workflow run ci.yml --ref api-spec-update`).

To set up the app (once):

1. Create a GitHub App owned by the `byteally` organisation (*Organization
   settings → Developer settings → GitHub Apps → New GitHub App*):
   - any name, such as `typesafe-sdk-spec-sync`, and the repository URL as
     the homepage;
   - under *Webhook*, clear *Active*;
   - *Repository permissions*: *Contents* and *Pull requests*, both *Read and
     write*;
   - *Where can this GitHub App be installed?*: *Only on this account*.
2. On the app's page, note the *Client ID*, and under *Private keys*
   generate a key. A `.pem` file downloads.
3. Install the app (*Install App* in the app's settings) on the `byteally`
   organisation, for the `typesafe-sdk` repository only.
4. Store the client ID as a variable and the key as a secret:

   ```sh
   gh variable set SPEC_SYNC_APP_CLIENT_ID --repo byteally/typesafe-sdk --body <client-id>
   gh secret set SPEC_SYNC_APP_PRIVATE_KEY --repo byteally/typesafe-sdk < path/to/key.pem
   ```

   Then delete the downloaded `.pem` file.
5. Check the setup with `gh workflow run spec-drift.yml --repo byteally/typesafe-sdk`.
   The *create-github-app-token* and *Identify the app's bot account* steps
   must succeed. They run on every check, so a revoked key or an uninstalled
   app makes the daily run fail.

To check by hand:

```sh
scripts/sync-spec.sh --check          # report only
scripts/sync-spec.sh                  # replace the vendored copy
cabal test typesafe-ai-core           # see what needs updating
```

To try a specification without replacing the vendored copy, point the tests
at it:

```sh
TYPESAFE_OPENAPI_SPEC=/path/to/openapi.json cabal test typesafe-ai-core
```

Also read the [Python SDK changelog](https://docs.typesafe.ai/sdk/python/changelog)
and the API reference. Behaviour that the specification does not describe,
such as error bodies, rate limit headers or retry advice, only shows up
there.

### 2. Update the bindings

Work through the failing tests. The usual cases:

| The specification… | Failing test | Update |
| --- | --- | --- |
| adds a property to a schema | `encodes exactly the declared properties` | Add the field to the type in `TypeSafe.Wire` (named `<schema><Property>`), to both `toJSON` and `toEncoding`, and to `parseJSON` (`.:` if required, `.:?` otherwise). Add it to the generator in `test/TypeSafe/ArbitraryInstances.hs`. Then expose it in the typed layer where it makes sense. |
| makes a property required, or optional | `requires the required properties and only those` | Switch between `.:` and `.:?`, and between `a` and `Maybe a`. |
| changes a property's type | `encoded values validate against the schema` | Change the field's type. |
| adds a schema | `every schema is bound` | Add a type to `TypeSafe.Wire` and a `Binding` to `ConformanceSpec.hs`. |
| adds a question or answer type | `question constructors match the Question discriminator` | Add constructors to `Question` and `Answer`, then a smart constructor and answer type in `TypeSafe.Question`, like `noul`/`Noul`. |
| adds an endpoint | `every operation is bound` | Add a `Call` in `TypeSafe.Call`, and add it to `sdkOperations` in `ConformanceSpec.hs`. |
| uses a new JSON Schema keyword | `the schema validator understands every keyword in use` | Teach `test/TypeSafe/JsonSchema.hs` the keyword. |
| changes `info.version` | `apiSpecVersion matches the vendored specification` | Update `apiSpecVersion`. |

Then:

- document new fields and functions with Haddock comments, with a doctest
  example where possible;
- add unit tests in `test/TypeSafe/QuestionSpec.hs` for typed behaviour;
- update the changelogs and the version table in `README.md`;
- bump the versions (see above) in both `.cabal` files, and the
  `typesafe-ai-core` bound in `typesafe-ai.cabal`.

## Releasing

One-time setup:

1. Create a Hackage account with upload rights for both packages. The first
   upload of a package name claims it.
2. Create an API token on Hackage (*Account* → *Manage API tokens*) and add
   it to the repository as the `HACKAGE_TOKEN` secret of a `hackage`
   environment. Requiring a reviewer on that environment is a good idea.

For each release:

1. Update `CHANGELOG.md` in both packages. The release notes are taken from
   the `## <version>` section of `typesafe-ai/CHANGELOG.md`.
2. Set `version:` in both `.cabal` files and update the dependency bound.
3. Check locally:

   ```sh
   cabal test all
   (cd typesafe-ai-core && cabal check) && (cd typesafe-ai && cabal check)
   ```

4. Tag and push: `git tag v0.1.0.0 && git push origin v0.1.0.0`. The
   `release` workflow tests, packages and uploads both packages and their
   documentation as Hackage **candidates**.
5. Review the candidates on Hackage.
6. Run the `release` workflow by hand with *publish* checked. It publishes
   `typesafe-ai-core`, then `typesafe-ai`, and creates the GitHub release.

To release from a workstation instead:

```sh
cabal sdist typesafe-ai-core typesafe-ai
cabal haddock typesafe-ai-core typesafe-ai --haddock-for-hackage
for p in typesafe-ai-core typesafe-ai; do
  cabal upload --publish --token "$HACKAGE_TOKEN" dist-newstyle/sdist/$p-*.tar.gz
  cabal upload --publish --documentation --token "$HACKAGE_TOKEN" dist-newstyle/$p-*-docs.tar.gz
done
```

## Conventions

- Commit messages, pull requests and issues describe the change for human
  readers. They do not carry tool or assistant attribution trailers.
- Warnings are errors in the project build (`cabal.project`), but not in the
  released packages, so that a new GHC warning never breaks a user's build.
