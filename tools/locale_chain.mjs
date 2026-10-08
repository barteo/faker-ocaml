// The fallback chain faker-js uses for a locale's prebuilt instance, derived from its code:
// de_AT -> de -> en -> base. gen_locale.mjs checks this against faker-js for every locale.
import { allLocales } from '@faker-js/faker';

export function chainOf(code) {
  if (code === 'base') return ['base'];
  if (code === 'en') return ['en', 'base'];
  const parts = code.split('_');
  const chain = [code];
  for (let i = parts.length - 1; i > 0; i--) {
    const parent = parts.slice(0, i).join('_');
    if (allLocales[parent] && parent !== 'en') chain.push(parent);
  }
  return [...chain, 'en', 'base'];
}
