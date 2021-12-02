const readline = require("readline");
const fs = require("fs");

const [filePath] = process.argv.slice(-1);

const createStreamAndRl = (filePath) =>
    readline.createInterface({
        input: fs.createReadStream(filePath),
        crlfDelay: Infinity,
    });

const valid = (rule, letter, pwd) => {
    const [pos1, pos2] = rule.split("-");

    return pwd.charAt(pos1 - 1) === letter && pwd.charAt(pos2 - 1) !== letter
        ? true
        : pwd.charAt(pos1 - 1) !== letter && pwd.charAt(pos2 - 1) === letter
        ? true
        : false;
};

(async () => {
    const rl = createStreamAndRl(filePath);
    let ok = 0;

    for await (const line of rl) {
        const [rule, letter, code] = line.split(" ");
        ok = valid(rule, letter.slice(0, -1), code) ? ok + 1 : ok;
    }
    console.log("22. ", ok);
})();
