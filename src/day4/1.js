const readline = require("readline");
const fs = require("fs");

const [filePath] = process.argv.slice(-1);

const createArr = async (filePath) => {
    let rl = readline.createInterface({
        input: fs.createReadStream(filePath),
        crlfDelay: Infinity,
    });

    let passports = [];
    let tmp = null;

    for await (const line of rl) {
        if (line === "") {
            passports.push(tmp);
            tmp = null;
        } else {
            tmp = tmp === null ? line : tmp + " " + line;
        }
    }
    passports.push(tmp);

    return passports;
};

(async () => {
    const inputArr = await createArr(filePath);
    const must = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

    let validPassports = 0;

    for (const passport of inputArr) {
        const vars = passport.split(" ");

        if (vars.length < 7) {
            continue;
        }

        let shallNotPass = false;
        for (const m of must) {
            if (!passport.includes(m + ":")) {
                shallNotPass = true;
                continue;
            }
        }
        if (shallNotPass) {
            continue;
        }

        validPassports = validPassports + 1;
    }

    console.log(`41. total: ${inputArr.length} valid: ${validPassports}`);
})();
