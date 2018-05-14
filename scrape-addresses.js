var request = require('request');
var cheerio = require('cheerio');
var fs = require('fs');

async function getContractsFromPage(pageN) {
  return new Promise((resolve, reject) => {
    request(`https://etherscan.io/contractsVerified/${pageN}`, function(error, response, body) {
      if(error) {
        console.log("Error: " + error);
        reject(error)
      }
      // console.log("Status code: " + response.statusCode);

      var $ = cheerio.load(body);

      const t = $('table>tbody')[0];
      let rows = $(t).find("tr");
      let arr = rows.toArray();
      let addresses = [];
      for (const r of arr) {
        const cell = $(r).find("td")[0];
        const address = $(cell).text().trim();

        const nameCell = $(r).find("td")[1];
        const name = $(nameCell).text().trim();
        // console.log(address);
        const s = (address+ " " + name + "\n");
        console.log(s)
        addresses += s;
      }
      resolve(addresses);
    });
  })
}


async function getAllContracts() {
  let addresses = [];
  for (let i = 1; i <= 1042; i++) {
    console.log(i)
    const contracts = await getContractsFromPage(i);
    addresses = addresses + contracts;
  }
  return addresses;
}

getAllContracts().then((addresses) => {
  fs.writeFile("verified-addresses.txt", addresses, ()=>{console.log("done")})
})