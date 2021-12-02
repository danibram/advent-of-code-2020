const readline = require("readline");
const fs = require("fs");

const [filePath] = process.argv.slice(-1);

let db = {};

const createStreamAndRl = (filePath) =>
    readline.createInterface({
        input: fs.createReadStream(filePath),
        crlfDelay: Infinity,
    });

(async () => {
    const rl = createStreamAndRl(filePath);

    for await (const line of rl) {
        db[parseInt(line)] = 1;
    }

    const rl2 = createStreamAndRl(filePath);

    for await (const line2 of rl2) {
        let n = parseInt(line2);
        let rest = 2020 - n;

        if (db[rest]) {
            console.log("1. ", n, rest, "*=", n * rest);
            return;
        }
    }
})();
