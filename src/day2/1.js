const readline = require("readline");
const fs = require("fs");

const [filePath] = process.argv.slice(-1);

const createStreamAndRl = (filePath) =>
    readline.createInterface({
        input: fs.createReadStream(filePath),
        crlfDelay: Infinity,
    });

const valid = (rule, letter, pwd) => {
    const [min, max] = rule.split("-");
    let rep = 0;

    for (let c of pwd) {
        rep = c === letter ? rep + 1 : rep;
    }
    return rep >= min && rep <= max;
};

(async () => {
    const rl = createStreamAndRl(filePath);
    let ok = 0;

    for await (const line of rl) {
        const [rule, letter, code] = line.split(" ");
        ok = valid(rule, letter.slice(0, -1), code) ? ok + 1 : ok;
    }
    console.log("21. ", ok);
})();
