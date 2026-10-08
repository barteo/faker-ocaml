# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

An OCaml (>= 5.0) port of `@faker-js/faker` **10.6.0** with **seed-for-seed output parity**: for the same seed, every method must return exactly what faker-js returns. That one rule decides most changes. Port upstream methods line by line, keeping every RNG call in the same order with the same arguments, and don't "simplify" or "fix" behaviour. Behaviour and data fixes belong upstream in faker-js. Read `docs/PORTING.md` before porting or changing a method. `CONTRIBUTING.md` covers the test workflow in detail.

Upstream reference: the faker-js source at the [`v10.6.0` tag](https://github.com/faker-js/faker/tree/v10.6.0/src) (`src/modules/<m>/module.ts`, `src/locales/...`). Fetch raw files from `raw.githubusercontent.com/faker-js/faker/v10.6.0/...`. The scratchpad path in `docs/PORTING.md` points to an earlier session's temporary directory and may no longer exist. `tools/node_modules/@faker-js/faker` holds only the compiled `dist/`.

## Commands

```sh
eval $(opam env --switch=default)   # if dune isn't on PATH
dune build
dune test                           # all parity fixtures + property tests
ONLY=person dune test --force       # one parity group (--force: dune doesn't track the env var)
ONLY='sweep_*' dune test --force    # prefix match: sweep_de, l10n_ja, ...
dune fmt                            # run before committing (.ocamlformat)
dune exec ./bin/demo.exe -- 42 ja   # one value per module, seed 42, locale ja
```

Fixture and data generation (Node, from the real faker-js pinned in `tools/package.json`). Run these inside `tools/` after `npm install`:

```sh
node gen_fixtures.mjs <group>       # tools/cases/<group>.mjs -> test/expected/expected_<group>.ml
FIXTURE_SEEDS=20 node gen_fixtures.mjs sweep   # wider temporary check; regenerate without it before committing
node gen_locale.mjs                 # regenerates all of lib/locales/
node gen_unicode.mjs                # lib/internal/unicode_{table,case,ccc}.ml
```

## Generated files: don't edit by hand

- `lib/locales/*` (locale data `<code>_data.ml`, chains/instances `locale_<code>.ml`, `faker_locales.ml`, `faker_all_locales.ml`)
- `test/expected/*.ml`
- `lib/internal/unicode_{table,case,ccc}.ml`, `lib/modules/fk_internet_char_mappings.ml`, `lib/modules/fk_internet_nfkd.ml`
- `faker.opam` (edit `dune-project` instead)

Commit regenerated files together with the change that produced them.

## Architecture

- `lib/dune` and `test/dune` use `(include_subdirs unqualified)`, so every `.ml` under `lib/` (or `test/`) is a top-level module in one flat namespace. Module ports are prefixed `fk_` so they don't clash with Stdlib names like `String`.
- **Instance**: `Core.t` (`lib/core.ml`) bundles a `Randomizer.t` (MersenneTwister19937 in `lib/internal/mersenne.ml`), the merged locale definitions (a `Json.t`) and the default reference date. Every public function takes it as the **last** argument. Errors go through `Core.error`, which raises `Faker_error` with faker-js's exact message.
- **Modules**: `lib/modules/fk_<m>.ml` ports upstream `module.ts`. Locale data is read through `Locale.get/find/strings` (`lib/locale.ml`), and `{{...}}` patterns are expanded by `Fake` (`lib/modules/fake.ml`, i.e. `helpers.fake`).
- **Public API**: `lib/faker.ml` re-exports each port as `Faker.<Module>`. Some are composed from several files, e.g. `Number` = `Fk_number` + `Fk_number_bigint`, and `Helpers` = `Fk_helpers` + `Fk_helpers_regexp` + `Fake`.
- **Registry**: every `fk_<m>.ml` ends with `let registry : (string * Registry.fn) list`, mapping each public method's camelCase name to a JSON-args wrapper built with `Args`. `lib/faker.ml` registers them all with `Registry.add`. `helpers.fake("{{module.method}}")` dispatches through it, and so do the locale sweep tests. A method missing from the registry breaks both.
- **Locales**: each locale is a separate module, so a program links only the locales it references. `Faker.All_locales` links all 77.

## Tests

- **Parity** (`test/test_parity.ml`): each case id is defined twice, in `tools/cases/<m>.mjs` (the JS call) and in `test/cases/cases_<m>.ml` (the OCaml closure returning `Json.t`, with helpers from `test/cases/t.ml`). The OCaml result is compared with the JSON that faker-js produced in `test/expected/expected_<m>.ml`. Cases run at seeds 42, 1337 and 7, three calls each, with the ref date fixed at 2025-01-01. Thrown errors are compared as `{"error": msg}`. A **new group** also needs an entry in `test/groups.ml`.
- **Sweeps**: `tools/cases/sweep.mjs` (every method with default options) and `tools/cases/l10n.mjs` (locale-sensitive options) produce one fixture per locale. `test/cases/cases_sweep.ml` evaluates them through the registry, so there is no OCaml case to write.
- **Properties** (`test/test_props.ml`): invariants over 1000 random seeds. `for_locales` runs them in every locale.

## Parity pitfalls (details in docs/PORTING.md)

- Never write `a *. b +. c` (or the `-.` variants). arm64 fuses it into an FMA. Use `Js.mul a b +. c`.
- OCaml evaluates function arguments and record fields **right to left**. Bind each random value with `let` in upstream order.
- JS strings are UTF-16. Use `Unicode.js_upper/js_lower/js_length`, `Js.code_points` and `Fk_internet_nfkd.nfkd`, never `String.*_ascii`, on locale data.
- JS number semantics live in `lib/internal/js.ml` (`number_to_string`, `toFixed`, `parseInt`, ...). `Math.round(x)` is `Float.floor (x +. 0.5)`. JS `bigint` is `Bigint.t`, with JS `div`/`rem` semantics.
