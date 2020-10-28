const https = require('https');
const fs = require('fs');

const vs16RegExp = /\uFE0F/g;
// avoid using a string literal like '\u200D' here because minifiers expand it inline
const zeroWidthJoiner = String.fromCharCode(0x200d);

function toCodePoints(rawEmoji) {
  const unicodeSurrogates = (rawEmoji.indexOf(zeroWidthJoiner) < 0 ? rawEmoji.replace(vs16RegExp, '') : rawEmoji);
  const points = [];
  let char = 0;
  let previous = 0;
  let i = 0;
  while (i < unicodeSurrogates.length) {
    char = unicodeSurrogates.charCodeAt(i++);
    if (previous) {
      points.push((0x10000 + ((previous - 0xd800) << 10) + (char - 0xdc00)).toString(16));
      previous = 0;
    } else if (char > 0xd800 && char <= 0xdbff) {
      previous = char;
    } else {
      points.push(char.toString(16));
    }
  }
  return points.join('-');
}

// Source emoji array: https://raw.githubusercontent.com/github/gemoji/master/db/emoji.json
// Format for each emoji:
// {
//   "emoji": "😀",
//   "description": "grinning face",
//   "category": "Smileys & Emotion",
//   "aliases": [
//     "grinning"
//   ],
//   "tags": [
//     "smile",
//     "happy"
//   ],
//   "unicode_version": "6.1",
//   "ios_version": "6.0"
// }
//
// Source emoji.json: https://unpkg.com/emoji.json@13.0.0/emoji.json
// Format:
// [
//   {
//     codes: '1F604',
//     char: '😄',
//     name: 'grinning face with smiling eyes',
//     category: 'Smileys & Emotion'
//   }
// ]

https.get(
  'https://unpkg.com/emoji.json@13.0.0/emoji.json',
  response => {
    let data = '';
    response.on('data', chunk => data += chunk);
    response.on('end', () => {
      const sourceEmojis = JSON.parse(data);
      console.log(`${sourceEmojis.length} emojis found`);

      const transformedEmojis = sourceEmojis.reduce((acc, curr) => {
        // const lowerDescription = curr.description.toLowerCase();
        // const keywords = curr.aliases.concat(curr.tags);
        // const filteredKeywords = keywords.filter(a => !a.includes('_') && !lowerDescription.includes(a.toLowerCase()));
        // acc[curr.emoji] = {
        acc[curr.char] = {
          style: 'unicode',
          // image: `${toCodePoints(curr.emoji)}.png`,
          image: `${toCodePoints(curr.char)}.png`,
          // name: `${curr.description}${filteredKeywords.length ? ` [${filteredKeywords.join(', ')}]` : ''} (${curr.category})`
          name: `${curr.name} - ${curr.category}`
        }
        return acc;
      }, {});

      fs.writeFile('./emoji.json', JSON.stringify(transformedEmojis, null, 2), function (err) {
        if (err) return console.log(err);
        console.log('Wrote emoji.json');
      });
    });
  }
);
