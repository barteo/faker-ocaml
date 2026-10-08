export const cases = [
  ['album', (f) => f.music.album()],
  ['artist', (f) => f.music.artist()],
  ['genre', (f) => f.music.genre()],
  ['songName', (f) => f.music.songName()],
  ['fake', (f) => f.helpers.fake('{{music.album}}|{{music.artist}}|{{music.genre}}|{{music.songName}}|')],
];
