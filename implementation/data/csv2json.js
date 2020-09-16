const csv = require('csv-parser')
const fs = require('fs')
const results = [];

fs.createReadStream('./kc_house_data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
  let subSample = results.filter((d,i) => i % 100 === 1);
  fs.writeFileSync("sample_kc_house_data.json", JSON.stringify(subSample, null, 2));

});
