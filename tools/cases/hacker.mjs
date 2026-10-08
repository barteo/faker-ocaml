export const cases = [
  ['abbreviation', (f) => f.hacker.abbreviation()],
  ['adjective', (f) => f.hacker.adjective()],
  ['noun', (f) => f.hacker.noun()],
  ['verb', (f) => f.hacker.verb()],
  ['ingverb', (f) => f.hacker.ingverb()],
  ['phrase', (f) => f.hacker.phrase()],
  ['fake', (f) => f.helpers.fake('{{hacker.noun}} {{hacker.phrase}}')],
];
