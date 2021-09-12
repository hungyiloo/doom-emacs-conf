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

// Source emoji.json: https://unpkg.com/emoji.json@13.1.0/emoji.json
// Format:
// [
//   {
//     codes: '1F604',
//     char: '😄',
//     name: 'grinning face with smiling eyes',
//     category: 'Smileys & Emotion'
//   }
// ]

const emojiKeywordsPromise  = new Promise(resolve => https.get(
  'https://raw.githubusercontent.com/unicode-org/cldr/master/common/annotations/en.xml',
  response => {
    let data = '';
    response.on('data', chunk => data += chunk);
    response.on('end', () => {
      const emojiKeywords = {};
      const annotationRegex = /<annotation cp="(?<emoji>[^"]+)">(?<keywords>.+)<\/annotation>/;
      data.split('\n').forEach(line => {
        const matches = line.match(annotationRegex);
        if (!matches) return;
        emojiKeywords[matches.groups.emoji] = matches.groups.keywords.split('|').map(keyword => keyword.trim());
      })
      resolve(emojiKeywords);
    });
  }
));

const transformedEmojisPromise = new Promise(resolve => https.get(
  'https://unpkg.com/emoji.json@13.1.0/emoji.json',
  response => {
    let data = '';
    response.on('data', chunk => data += chunk);
    response.on('end', () => {
      const sourceEmojis = JSON.parse(data);
      console.log(`${sourceEmojis.length} emojis found`);

      const transformedEmojis = sourceEmojis.reduce((acc, curr) => {
        acc[curr.char] = {
          style: 'unicode',
          image: `${toCodePoints(curr.char)}.png`,
          // name: `${curr.name} - ${curr.category}`
          name: curr.name
        }
        return acc;
      }, {});

      resolve(transformedEmojis);
    });
  }
));

Promise.all([
  emojiKeywordsPromise,
  transformedEmojisPromise
]).then(([emojiKeywords, transformedEmojis]) => {
  Object.keys(transformedEmojis).forEach(emoji => {
    const keywords = emojiKeywords[emoji];
    if (!keywords) return;
    const existingName = transformedEmojis[emoji].name;
    const validKeywords = keywords.filter(k => !existingName.includes(k)).join(', ');
    if (validKeywords.length === 0) return;
    transformedEmojis[emoji].name = `${existingName} [${keywords.filter(k => !existingName.includes(k)).join(', ')}]`;
  });
  fs.writeFile('./emoji.json', JSON.stringify(transformedEmojis, null, 2), function (err) {
    if (err) return console.log(err);
    console.log('Wrote emoji.json');
  });
})
