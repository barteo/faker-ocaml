// Locale sweep: every module method with its default options, in every locale.
// The OCaml side (test/cases/cases_sweep.ml) calls the same "module.method" through the
// helpers.fake registry, so this also checks each registry entry's defaults.
import { Faker, allLocales, base } from '@faker-js/faker';

export const locales = Object.keys(allLocales);
export const seeds = [42, 1337, 7];
export const runs = 2;

// helpers methods need arguments.
const SKIP_MODULES = new Set(['helpers']);
const SKIP_METHODS = new Set([
  // Deprecated and noisy (it warns on every call).
  'image.urlLoremFlickr',
  // Required arguments: faker-js throws a TypeError without them.
  'date.between',
  'date.betweens',
  'string.fromCharacters',
]);

// Public methods along the prototype chain (e.g. DateModule extends SimpleDateModule).
const methods = (obj) => {
  const names = new Set();
  for (let p = Object.getPrototypeOf(obj); p && p !== Object.prototype; p = Object.getPrototypeOf(p))
    for (const name of Object.getOwnPropertyNames(p))
      if (name !== 'constructor' && typeof p[name] === 'function') names.add(name);
  return [...names].sort();
};

const probe = new Faker({ locale: base });
export const cases = Object.keys(probe)
  .filter((m) => m !== 'fakerCore' && !SKIP_MODULES.has(m))
  .sort()
  .flatMap((m) =>
    methods(probe[m])
      .filter((name) => !SKIP_METHODS.has(`${m}.${name}`))
      .map((name) => [`${m}.${name}`, (f) => f[m][name]()]),
  )
  .concat([['faker.getMetadata', (f) => f.getMetadata()]]);
