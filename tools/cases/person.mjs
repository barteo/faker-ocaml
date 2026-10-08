const sexes = [['', undefined], ['/female', 'female'], ['/male', 'male'], ['/generic', 'generic']];
export const cases = [];
for (const m of ['firstName', 'lastName', 'middleName', 'prefix'])
  for (const [suffix, sex] of sexes)
    cases.push([m + suffix, (f) => (sex === undefined ? f.person[m]() : f.person[m](sex))]);
cases.push(
  ['fullName', (f) => f.person.fullName()],
  ['fullName/female', (f) => f.person.fullName({ sex: 'female' })],
  ['fullName/male', (f) => f.person.fullName({ sex: 'male' })],
  ['fullName/generic', (f) => f.person.fullName({ sex: 'generic' })],
  ['fullName/first', (f) => f.person.fullName({ firstName: 'Joann' })],
  ['fullName/last', (f) => f.person.fullName({ lastName: 'Doe' })],
  ['fullName/both', (f) => f.person.fullName({ firstName: 'Jane', lastName: 'Doe', sex: 'female' })],
  ['fullName/empty', (f) => f.person.fullName({ firstName: '', lastName: '' })],
  ['fullName/dollar', (f) => f.person.fullName({ firstName: '$& $1', lastName: '$$' })],
  ['gender', (f) => f.person.gender()],
  ['sex', (f) => f.person.sex()],
  ['sexType', (f) => f.person.sexType()],
  ['sexType/generic', (f) => f.person.sexType({ includeGeneric: true })],
  ['sexType/nogeneric', (f) => f.person.sexType({ includeGeneric: false })],
  ['bio', (f) => f.person.bio()],
  ['suffix', (f) => f.person.suffix()],
  ['jobTitle', (f) => f.person.jobTitle()],
  ['jobDescriptor', (f) => f.person.jobDescriptor()],
  ['jobArea', (f) => f.person.jobArea()],
  ['jobType', (f) => f.person.jobType()],
  ['zodiacSign', (f) => f.person.zodiacSign()],
  ['fake/names', (f) => f.helpers.fake('{{person.firstName}}|{{person.firstName(female)}}|{{person.lastName(male)}}|{{person.middleName(generic)}}|{{person.prefix(male)}}|{{person.fullName}}|{{person.fullName({"sex":"female","firstName":"X"})}}')],
  ['fake/misc', (f) => f.helpers.fake('{{person.gender}}|{{person.sex}}|{{person.sexType}}|{{person.sexType({"includeGeneric":true})}}|{{person.suffix}}|{{person.jobTitle}}|{{person.jobDescriptor}}|{{person.jobArea}}|{{person.jobType}}|{{person.zodiacSign}}|{{person.lastName}}|{{person.middleName}}|{{person.prefix}}')],
  ['fake/bio', (f) => f.helpers.fake('{{person.bio}}')],
);
