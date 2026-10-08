// The Faker / SimpleFaker instance API (not a module).
import { Faker, SimpleFaker, en, base, mergeLocales, de, de_AT } from '@faker-js/faker';

export const cases = [
  ['create/emptyLocale', () => new Faker({ locale: [] })],
  [
    'create/defaultRefDate',
    (f) =>
      new Faker({
        locale: [en, base],
        seed: f.number.int(1000),
        config: { defaultRefDate: () => new Date('2020-06-15T12:00:00.000Z') },
      }).date.recent(),
  ],
  [
    'setDefaultRefDate/string',
    (f) => {
      f.setDefaultRefDate('2020-02-02T00:00:00.000Z');
      return f.date.past();
    },
  ],
  [
    'setDefaultRefDate/number',
    (f) => {
      f.setDefaultRefDate(1577836800000);
      return f.date.soon();
    },
  ],
  ['seed', (f) => [f.seed(123), f.number.int()]],
  ['simpleFaker', (f) => {
    const s = new SimpleFaker({ seed: f.number.int(1000) });
    return [s.number.int(), s.string.uuid(), s.datatype.boolean(), s.helpers.arrayElement(['a', 'b'])];
  }],
  ['mergeLocales', () => {
    const m = mergeLocales([de_AT, de, en, base]);
    return [Object.keys(m), Object.keys(m.person), m.person.first_name.female.slice(0, 3)];
  }],
  ['definitions/missing', () => new Faker({ locale: [base] }).definitions.person.first_name],
];
