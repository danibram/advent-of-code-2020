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

let rules = {
    byr: (number) =>
        number.length === 4 &&
        parseInt(number) !== NaN &&
        parseInt(number) >= 1920 &&
        parseInt(number) <= 2002,
    iyr: (number) =>
        number.length === 4 &&
        parseInt(number) !== NaN &&
        parseInt(number) >= 2010 &&
        parseInt(number) <= 2020,
    eyr: (number) =>
        number.length === 4 &&
        parseInt(number) !== NaN &&
        parseInt(number) >= 2020 &&
        parseInt(number) <= 2030,
    hgt: (number) => {
        let last2 = number.slice(0, -2);
        if (last2 !== "in" || last2 !== "cm") {
            return false;
        }
        if (last2 === "in") {
            let [number, _] = number.split("in");
            if (parseInt(number) === NaN) {
                return false;
            }
            if (parseInt(number) < 59 || parseInt(number) > 76) {
                return false;
            }
            return true;
        }

        if (last2 === "cm") {
            let [number, _] = number.split("cm");
            if (parseInt(number) === NaN) {
                return false;
            }
            if (parseInt(number) < 150 || parseInt(number) > 193) {
                return false;
            }
            return true;
        }
    },
    hcl: (string) => {
        if (string.charAt(0) !== "#") {
            return false;
        }
        let rest = string.slice(1);
        if (rest.length !== 6) {
            return false;
        }

        rest.test(/^([a-f])+$/);
    },
}(async () => {
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

        shallNotPass = false;
        const data = passport.split(" ");
        for (const d of data) {
            let [key, value] = d;
            if (key === "cid") continue;

            if (key === "byr") {
            }
        }
        if (shallNotPass) {
            continue;
        }

        validPassports = validPassports + 1;
    }

    console.log("41. ", validPassports);
})();
