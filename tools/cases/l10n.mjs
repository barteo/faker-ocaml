// Locale-sensitive options, in every locale. Each id is also the OCaml call: a
// "module.method(args)" expression evaluated through the helpers.fake registry
// (test/cases/cases_sweep.ml), so the args must be JSON.
import { allLocales } from '@faker-js/faker';

export const locales = Object.keys(allLocales);
export const runs = 2;

const call = (expr) => {
  const open = expr.indexOf('(');
  const [m, name] = expr.slice(0, open).split('.');
  const args = JSON.parse(`[${expr.slice(open + 1, -1)}]`);
  return [expr, (f) => f[m][name](...args)];
};

export const cases = [
  'person.firstName("female")',
  'person.firstName("male")',
  'person.lastName("female")',
  'person.lastName("male")',
  'person.middleName("female")',
  'person.fullName({"sex":"female"})',
  'person.fullName({"sex":"male"})',
  'person.fullName({"firstName":"Anna"})',
  'person.prefix("female")',
  'person.prefix("male")',
  'person.suffix()',
  'location.zipCode({"state":"CA"})',
  'location.zipCode({"state":"ON"})',
  'location.zipCode("###")',
  'location.state({"abbreviated":true})',
  'location.streetAddress(true)',
  'location.streetAddress({"useFullAddress":true})',
  'location.countryCode("alpha-3")',
  'location.countryCode("numeric")',
  'location.timeZone()',
  'date.month({"abbreviated":true})',
  'date.month({"context":true})',
  'date.month({"abbreviated":true,"context":true})',
  'date.weekday({"abbreviated":true})',
  'date.weekday({"context":true})',
  'date.weekday({"abbreviated":true,"context":true})',
  'phone.number({"style":"human"})',
  'phone.number({"style":"national"})',
  'phone.number({"style":"international"})',
  'word.adjective({"length":{"min":3,"max":6}})',
  'word.noun({"length":5,"strategy":"closest"})',
  'word.verb({"length":20,"strategy":"shortest"})',
  'word.sample({"length":4,"strategy":"any-length"})',
  'word.words(5)',
  'lorem.word({"length":4,"strategy":"closest"})',
  'lorem.sentences(3)',
  'lorem.paragraphs(2)',
  'internet.email({"firstName":"Jürgen","lastName":"Ødegård"})',
  'internet.username({"firstName":"Анна","lastName":"Łukasz"})',
  'internet.displayName({"firstName":"Zoë"})',
  'internet.email({"allowSpecialCharacters":true})',
  'internet.domainWord()',
  'commerce.price({"symbol":"€"})',
  'commerce.productName()',
  'company.name()',
  'finance.currencyName()',
  'finance.creditCardNumber("visa")',
  'finance.creditCardNumber("mastercard")',
  'animal.type()',
  'system.fileName({"extensionCount":2})',
  'food.dish()',
  'music.songName()',
  'book.title()',
  'vehicle.vehicle()',
].map(call);
