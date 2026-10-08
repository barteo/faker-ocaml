export const cases = [
  ['number', (f) => f.phone.number()],
  ['number/human', (f) => f.phone.number({ style: 'human' })],
  ['number/national', (f) => f.phone.number({ style: 'national' })],
  ['number/international', (f) => f.phone.number({ style: 'international' })],
  ['number/mobile', (f) => f.phone.number({ style: 'mobile' })],
  ['number/err', (f) => f.phone.number({ style: 'nope' })],
  ['imei', (f) => f.phone.imei()],
  ['fake', (f) => f.helpers.fake('{{phone.number}}|{{phone.number({"style":"national"})}}|{{phone.number({"style":"international"})}}|{{phone.imei}}')],
];
