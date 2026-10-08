// JS string semantics the port emulates (lib/internal/unicode.ml, lib/internal/js.ml), checked
// directly against node. The instance is unused.
const samples = [
  'ΑΣ ΣΑ ΌΣΟΣ. Σ ΑΣ́ ΑΣ\'Α',
  'İstanbul KELVINK straße ǅemal ﬁnale ŉ',
  'ǈǋǲ ΐ ᾳ ﬃ 𐐨𐑐 𞤢 ⓐ Ⅻ',
  'հայերէն եւ և Ⴀ ა ꙁ',
];
const stacked = [
  'שָׁלוֹם', // Hebrew: shin dot (ccc 24) before qamats (18)
  'بَّبَّ', // Arabic: shadda (33) / fatha (30) both ways
  'á̴̖b̧́', // Latin marks of classes 230, 220, 1, 202
  'Ạ̊ ẫ ǖ ᾷ 가각 ㎏ 𝐇𝐞𝐥𝐥𝐨 ①',
];
const NUMBERS = ['', ' 12 ', '1e3', '0x1F', '0o17', '0b101', '.5', '5.', '+1', '-Infinity', 'NaN', '1_000', '0x', '١', '12px', ' \t\n'];

export const cases = [
  ['toUpperCase', () => samples.map((s) => s.toUpperCase())],
  ['toLowerCase', () => samples.map((s) => s.toLowerCase())],
  ['upperFirst', () => [...samples, 'ǆa', 'ßa', '𐐨x', 'ŉx'].map((s) => s.charAt(0).toUpperCase() + s.slice(1))],
  ['nfkd', () => [...samples, ...stacked].map((s) => s.normalize('NFKD'))],
  ['slugify', (f) => [...samples, ...stacked].map((s) => f.helpers.slugify(s))],
  ['username', (f) => stacked.map((s) => f.internet.username({ firstName: s, lastName: 'x' }))],
  ['number', () => NUMBERS.map((s) => String(Number(s)))],
];
