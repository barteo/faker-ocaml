export const cases = [
  ['boolean', (f) => f.datatype.boolean()],
  ['boolean/0.9', (f) => f.datatype.boolean(0.9)],
  ['boolean/0.1', (f) => f.datatype.boolean({ probability: 0.1 })],
  ['boolean/1', (f) => f.datatype.boolean(1)],
];
