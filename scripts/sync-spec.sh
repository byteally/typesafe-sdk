#!/usr/bin/env bash
# Compare the published TypeSafe OpenAPI specification with the vendored copy
# in typesafe-ai-core/spec/openapi.json, and optionally replace it.
#
# Usage:
#   scripts/sync-spec.sh             replace the vendored copy if the spec changed
#   scripts/sync-spec.sh --check     only report; exit 1 if the spec changed
#   scripts/sync-spec.sh --summary FILE
#                                    also write a Markdown summary to FILE
#
# The specification is normalised with `jq -S` (sorted keys, fixed
# indentation) so that diffs only show real changes. After replacing it, run
#
#   cabal test typesafe-ai-core
#
# The conformance tests then name every operation, schema, property and
# keyword the bindings do not cover yet. See MAINTAINING.md.
#
# Requires curl and jq. Set TYPESAFE_OPENAPI_URL to use another URL.
set -euo pipefail

url="${TYPESAFE_OPENAPI_URL:-https://api.typesafe.ai/openapi.json}"
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
vendored="$root/typesafe-ai-core/spec/openapi.json"

check=false
summary=""
while [ $# -gt 0 ]; do
  case "$1" in
    --check) check=true ;;
    --summary) summary="$2"; shift ;;
    -h | --help) sed -n '2,20p' "$0"; exit 0 ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
  shift
done

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

curl --fail --silent --show-error --location "$url" | jq -S . > "$work/published.json"

# One line per structural fact of a specification, so that `diff` shows what
# was added and removed. Property lines include the property's schema without
# its documentation, so a type change shows up as a removal plus an addition.
outline() {
  jq -r '
    def shape: del(.description, .examples, .title) | tojson;
    "version \(.info.version)",
    (.paths | to_entries[] | .key as $path | .value | keys[] | "operation \(ascii_upcase) \($path)"),
    (.components.schemas | to_entries[] | .key as $schema | .value |
      "schema \($schema)",
      ((.properties // {}) | to_entries[] | "property \($schema).\(.key): \(.value | shape)"),
      ((.required // [])[] | "required \($schema).\(.)"),
      ((.discriminator.mapping // {}) | keys[] | "variant \($schema).\(.)"))
  ' "$1" | LC_ALL=C sort
}

old_version="$(jq -r .info.version "$vendored")"
new_version="$(jq -r .info.version "$work/published.json")"

if cmp --silent "$vendored" "$work/published.json"; then
  echo "The vendored specification is up to date (version $old_version)."
  [ -n "$summary" ] && : > "$summary"
  exit 0
fi

outline "$vendored" > "$work/old.txt"
outline "$work/published.json" > "$work/new.txt"
added="$(LC_ALL=C comm -13 "$work/old.txt" "$work/new.txt")"
removed="$(LC_ALL=C comm -23 "$work/old.txt" "$work/new.txt")"

report() {
  echo "## TypeSafe API specification changed"
  echo
  echo "Vendored version: \`$old_version\`, published version: \`$new_version\` ($url)."
  echo
  if [ -z "$added$removed" ]; then
    echo "Only documentation changed (descriptions, titles or examples)."
  else
    if [ -n "$added" ]; then
      echo "### Added"
      echo
      printf '%s\n' "$added" | sed 's/^/- `/; s/$/`/'
      echo
    fi
    if [ -n "$removed" ]; then
      echo "### Removed"
      echo
      printf '%s\n' "$removed" | sed 's/^/- `/; s/$/`/'
      echo
    fi
  fi
}

report
[ -n "$summary" ] && report > "$summary"

if $check; then
  exit 1
fi

cp "$work/published.json" "$vendored"
echo
echo "Updated $vendored. Now run: cabal test typesafe-ai-core"
