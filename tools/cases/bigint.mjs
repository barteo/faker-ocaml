// Bigint arithmetic and BigInt(...) conversions, checked against JS BigInt;
// the faker argument is unused. Keep the lists in sync with
// test/cases/cases_bigint.ml.
export const seeds = [0];
export const runs = 1;

const OPERANDS = [
  '0', '1', '-1', '7', '-7', '9999', '-10000',
  '123456789012345678901234567890', '-98765432109876543210',
  '4611686018427387903', '-4611686018427387904', '4611686018427387904',
  '100000000000000000000000000000000000000000',
];
const OPS = {
  add: (a, b) => a + b,
  sub: (a, b) => a - b,
  mul: (a, b) => a * b,
  div: (a, b) => a / b,
  rem: (a, b) => a % b,
  cmp: (a, b) => (a < b ? -1n : a > b ? 1n : 0n),
};
const INT_MIN = -(2n ** 62n);
const INT_MAX = 2n ** 62n - 1n;

const STRINGS = [
  '', ' ', '  12 ', ' 12﻿', '\n7\t', '+5', '-5', '-0', '007', '0x1F', '0X1f',
  '0x00ff', '0o17', '0b101', '-0x10', '1e3', '1_000', '0x', '0b2', '-', '+', '12abc', 'abc',
  '99999999999999999999999999999999999999', '-0000000000000000000000001',
  '0xffffffffffffffffffffffffffffffff',
];
const FLOATS = [
  0, -0, 1, -1, 42, 1e21, -1e21, 2 ** 53 + 2, 2 ** 62, -(2 ** 62), 2 ** 63,
  1.7976931348623157e308, -1e300, 1.5, -0.5, 5e-324, NaN, Infinity, -Infinity,
];

const run = (fn) => () => String(fn());
export const cases = [
  ...Object.entries(OPS).flatMap(([op, fn]) =>
    OPERANDS.flatMap((a) =>
      OPERANDS.filter((b) => !((op === 'div' || op === 'rem') && b === '0')).map((b) => [
        `${op}/${a}/${b}`,
        run(() => fn(BigInt(a), BigInt(b))),
      ])
    )
  ),
  ...OPERANDS.map((a) => [`neg/${a}`, run(() => -BigInt(a))]),
  ...OPERANDS.map((a) => [
    `to_int/${a}`,
    () => (BigInt(a) >= INT_MIN && BigInt(a) <= INT_MAX ? String(BigInt(a)) : null),
  ]),
  ['of_int/min', run(() => INT_MIN)],
  ['of_int/max', run(() => INT_MAX)],
  ...STRINGS.map((x, i) => [`of_string/${i}`, run(() => BigInt(x))]),
  ...FLOATS.map((x, i) => [`of_float/${i}`, run(() => BigInt(x))]),
  ['of_bool/true', run(() => BigInt(true))],
  ['of_bool/false', run(() => BigInt(false))],
];
