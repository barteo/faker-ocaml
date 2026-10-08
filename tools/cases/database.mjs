export const cases = [
  ['column', (f) => f.database.column()],
  ['type', (f) => f.database.type()],
  ['collation', (f) => f.database.collation()],
  ['engine', (f) => f.database.engine()],
  ['mongodbObjectId', (f) => f.database.mongodbObjectId()],
  ['fake', (f) => f.helpers.fake('{{database.column}} {{database.type}} {{database.mongodbObjectId}}')],
];
