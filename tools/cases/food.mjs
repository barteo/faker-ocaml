export const cases = [
  ['adjective', (f) => f.food.adjective()],
  ['description', (f) => f.food.description()],
  ['dish', (f) => f.food.dish()],
  ['ethnicCategory', (f) => f.food.ethnicCategory()],
  ['fruit', (f) => f.food.fruit()],
  ['ingredient', (f) => f.food.ingredient()],
  ['meat', (f) => f.food.meat()],
  ['spice', (f) => f.food.spice()],
  ['vegetable', (f) => f.food.vegetable()],
  ['fake', (f) => f.helpers.fake('{{food.adjective}}|{{food.description}}|{{food.dish}}|{{food.ethnicCategory}}|{{food.fruit}}|{{food.ingredient}}|{{food.meat}}|{{food.spice}}|{{food.vegetable}}|')],
  ['dish/many', (f) => f.helpers.multiple(() => f.food.dish(), { count: 40 })],
  ['description/many', (f) => f.helpers.multiple(() => f.food.description(), { count: 40 })],
];
