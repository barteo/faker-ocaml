import { generateMersenne32Randomizer, generateMersenne53Randomizer } from '@faker-js/faker';

// Raw randomizer sequences; the faker argument is unused.
export const seeds = [0];
export const runs = 1;

const seq = (gen, seed, n = 1000) => {
  const r = gen();
  r.seed(seed);
  return Array.from({ length: n }, () => r.next());
};

const SEEDS = [0, 1, 42, 1337, -1, 2 ** 31, 2 ** 32 + 5, 123456789012];
export const cases = [
  ...SEEDS.map((s) => [`f53/${s}`, () => seq(generateMersenne53Randomizer, s)]),
  ...SEEDS.map((s) => [`f32/${s}`, () => seq(generateMersenne32Randomizer, s)]),
  ['f53/[1,2,3]', () => seq(generateMersenne53Randomizer, [1, 2, 3])],
  ['f53/[42]', () => seq(generateMersenne53Randomizer, [42])],
  ['f32/[5,4294967295,7,8,9]', () => seq(generateMersenne32Randomizer, [5, 4294967295, 7, 8, 9])],
];
