export const cases = [
  ['name', (f) => f.company.name()],
  ['catchPhrase', (f) => f.company.catchPhrase()],
  ['buzzPhrase', (f) => f.company.buzzPhrase()],
  ['catchPhraseAdjective', (f) => f.company.catchPhraseAdjective()],
  ['catchPhraseDescriptor', (f) => f.company.catchPhraseDescriptor()],
  ['catchPhraseNoun', (f) => f.company.catchPhraseNoun()],
  ['buzzAdjective', (f) => f.company.buzzAdjective()],
  ['buzzVerb', (f) => f.company.buzzVerb()],
  ['buzzNoun', (f) => f.company.buzzNoun()],
  ['fake', (f) => f.helpers.fake('{{company.name}}|{{company.catchPhrase}}|{{company.buzzPhrase}}|{{company.catchPhraseAdjective}}|{{company.catchPhraseDescriptor}}|{{company.catchPhraseNoun}}|{{company.buzzAdjective}}|{{company.buzzVerb}}|{{company.buzzNoun}}')],
];
