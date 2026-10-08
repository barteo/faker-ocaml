export const cases = [
  ['author', (f) => f.book.author()],
  ['format', (f) => f.book.format()],
  ['genre', (f) => f.book.genre()],
  ['publisher', (f) => f.book.publisher()],
  ['series', (f) => f.book.series()],
  ['title', (f) => f.book.title()],
  ['fake', (f) => f.helpers.fake('{{book.author}}|{{book.format}}|{{book.genre}}|{{book.publisher}}|{{book.series}}|{{book.title}}|')],
];
