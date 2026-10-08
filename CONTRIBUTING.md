# Contributing

Thanks for wanting to help with the OCaml port of faker-js!

## Before you start

- For anything bigger than a small fix, please open an issue first so we can agree on the
  approach.
- The top rule of this project is **seed-for-seed parity with @faker-js/faker 10.6.0**. For the
  same seed, every method must return exactly what faker-js returns. If you change a method's
  behaviour, the change belongs upstream in faker-js, not here.
- Read the [Porting Guide](docs/PORTING.md). It lists the API conventions and the parity
  pitfalls (FMA contraction, UTF-16 vs UTF-8, OCaml's right-to-left argument evaluation, JS
  number formatting) that cause most fixture failures.

## Architecture

| Path | Purpose |
|---|---|
| `lib/faker.ml` | The public entry point. It exposes each module as `Faker.<Module>`. |
| `lib/core.ml`, `lib/randomizer.ml` | The faker instance and the MersenneTwister19937 randomizer. |
| `lib/modules/fk_<m>.ml` | The line-by-line port of upstream `src/modules/<m>/module.ts`. |
| `lib/internal/` | Shared helpers: JSON, JS number and string semantics, dates, Unicode. |
| `lib/locales/` | Generated locale modules: `<code>_data.ml` (data), `locale_<code>.ml` (chain and instance), `faker_locales.ml` (`Faker.Locales`) and `faker_all_locales.ml` (`Faker.All_locales`). **Don't edit them by hand.** |
| `tools/` | Node scripts that generate locale data and test fixtures from the real faker-js. |
| `test/` | Parity fixtures (`test_parity.ml`) and property tests (`test_props.ml`). |

When porting a method, use the faker-js source at the
[`v10.6.0` tag](https://github.com/faker-js/faker/tree/v10.6.0/src) as the reference.

## Sourcing data and adding a locale

All locale data comes from the faker-js npm package. Never add or fix data here by hand.
Data fixes belong in faker-js.

Every faker-js locale is already included. When the faker-js version changes (or it gains a
locale), regenerate them all:

```sh
cd tools && npm install && node gen_locale.mjs
```

This writes the data, fallback chain and prebuilt instance of every locale in
`lib/locales/`, plus the `Faker.Locales` and `Faker.All_locales` modules. It derives each
chain from the locale code (`de_AT -> de -> en -> base`) and fails if the result differs from
faker-js's own prebuilt instance. The locale sweeps (below) pick up new locales automatically.

## Building

You'll need OCaml >= 5.0 and Node.js. Node is only needed to regenerate fixtures.

```sh
opam install dune alcotest
dune build
```

## Testing

```sh
dune test                       # all parity fixtures + property tests
ONLY=person dune test --force   # one module's parity fixtures
```

### Adding tests for new methods/parameters

#### Parity (fixed-seed) tests

Each case is defined twice, under the same id:

1. In `tools/cases/<m>.mjs`, as the faker-js call:
   `['phrase', (f) => f.hacker.phrase()]`
2. In `test/cases/cases_<m>.ml`, as the OCaml call:
   `("phrase", fun f -> s (H.phrase f))` (the helpers are in `test/cases/t.ml`).

Then regenerate the expected values from faker-js:

```sh
cd tools && node gen_fixtures.mjs <m>   # writes test/expected/expected_<m>.ml
```

Each case runs for seeds 42, 1337 and 7, and calls the method three times in a row. Thrown
errors are compared too, so cover the invalid-input paths as well. A case file can override
`seeds` and `runs`, and can set `export const locale = 'de_AT'` to run in another locale.

#### Locale sweeps

Two parametric case files run in every locale. They export `locales`, so `gen_fixtures.mjs`
writes one fixture file per locale (`expected_sweep_de.ml`, ...) plus an index:

- `tools/cases/sweep.mjs` calls every method of every module (except `helpers`) with its
  default options. The ids are `module.method`.
- `tools/cases/l10n.mjs` calls methods with locale-sensitive options. The ids are
  `module.method(json args)`.

The OCaml side (`test/cases/cases_sweep.ml`) evaluates both kinds of id through the
`helpers.fake` registry, so there is nothing to write by hand. To add a locale-sensitive case,
add its expression to `l10n.mjs` and run `node gen_fixtures.mjs l10n`.

For a wider, temporary check, `FIXTURE_SEEDS=20 node gen_fixtures.mjs sweep` regenerates with
20 seeds. Regenerate without it before committing.

#### Property (random-seed) tests

Add invariants that must hold for any seed, such as "a UUID is 36 hex-and-dash characters", to
`test/test_props.ml`. They run for 1000 seeds. Invariants added with `for_locales` run in every
locale (30 seeds each).

## Committing

- Run `dune fmt` before committing. The project uses `.ocamlformat`.
- Commit generated files (`lib/locales/*_data.ml`, `test/expected/*.ml`, `faker.opam`) together
  with the change that produced them. `faker.opam` is generated from `dune-project`, so edit
  `dune-project` instead.
- Make sure `dune build` and `dune test` pass.
