const methods = ['adjective', 'adverb', 'conjunction', 'interjection', 'noun', 'preposition', 'verb', 'sample'];
const variants = [
  ['', undefined],
  ['/5', 5],
  ['/range', { length: { min: 4, max: 6 } }],
  ['/len3', { length: 3 }],
  ['/fail', { length: 50 }],
  ['/fail-range', { length: { min: 30, max: 40 } }],
  ['/closest', { length: 30, strategy: 'closest' }],
  ['/closest-low', { length: 1, strategy: 'closest' }],
  ['/closest-range', { length: { min: 25, max: 28 }, strategy: 'closest' }],
  ['/shortest', { strategy: 'shortest' }],
  ['/longest', { strategy: 'longest' }],
  ['/shortest-len', { length: 50, strategy: 'shortest' }],
  ['/longest-len', { length: { min: 40, max: 50 }, strategy: 'longest' }],
  ['/any-length', { length: 50, strategy: 'any-length' }],
  ['/any-length-nolen', { strategy: 'any-length' }],
  ['/fail-nolen', { strategy: 'fail' }],
  ['/closest-nolen', { strategy: 'closest' }],
  ['/match-closest', { length: { min: 5, max: 7 }, strategy: 'closest' }],
];
export const cases = [];
for (const m of methods)
  for (const [suffix, opts] of variants)
    cases.push([m + suffix, (f) => (opts === undefined ? f.word[m]() : f.word[m](opts))]);
cases.push(
  ['words', (f) => f.word.words()],
  ['words/5', (f) => f.word.words(5)],
  ['words/count', (f) => f.word.words({ count: 4 })],
  ['words/range', (f) => f.word.words({ count: { min: 2, max: 7 } })],
  ['words/0', (f) => f.word.words(0)],
  ['fake/noun', (f) => f.helpers.fake('{{word.noun}} {{word.verb(5)}} {{word.adjective({"length":{"min":3,"max":4}})}}')],
  ['fake/sample', (f) => f.helpers.fake('{{word.sample}} {{word.sample({"length":40,"strategy":"closest"})}}')],
  ['fake/misc', (f) => f.helpers.fake('{{word.adverb}}|{{word.conjunction}}|{{word.interjection}}|{{word.preposition}}|{{word.words(4)}}|{{word.words({"count":{"min":1,"max":2}})}}')],
  ['fake/strategy', (f) => f.helpers.fake('{{word.noun({"length":50,"strategy":"longest"})}}|{{word.verb({"strategy":"shortest"})}}|{{word.adjective({"length":50,"strategy":"any-length"})}}')],
);
