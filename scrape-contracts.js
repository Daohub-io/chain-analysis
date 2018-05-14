var request = require('request');
var cheerio = require('cheerio');
var fs = require('fs');

async function getContractsFromPage(address) {
  return new Promise((resolve, reject) => {
    request(`https://etherscan.io/address/${address}#code`, function(error, response, body) {
      if(error) {
        console.log("Error: " + error);
        reject(error)
      }
      // console.log("Status code: " + response.statusCode);

      var $ = cheerio.load(body);

      const t = $('#code>div>div>div>div>table>tbody>tr>td')[1];
      const name = $(t).text().trim();
      resolve(name);
    });
  })
}

getContractsFromPage(process.argv[2]).then(console.log)