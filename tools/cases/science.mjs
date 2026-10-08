export const cases = [
  ['chemicalElement', (f) => f.science.chemicalElement()],
  ['unit', (f) => f.science.unit()],
  ['unit/many', (f) => f.helpers.multiple(() => f.science.unit(), { count: 20 })],
  ['fake', (f) => f.helpers.fake('{{science.chemicalElement.name}} {{science.chemicalElement.atomicNumber}} {{science.unit.symbol}} {{science.chemicalElement}}')],
];
