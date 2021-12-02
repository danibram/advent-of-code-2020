const readline = require("readline");
const fs = require("fs");

const [filePath] = process.argv.slice(-1);

const createArr = async (filePath) => {
    let rl = readline.createInterface({
        input: fs.createReadStream(filePath),
        crlfDelay: Infinity,
    });

    let numbers = [];

    for await (const line of rl) {
        numbers.push(line);
    }

    return numbers;
};

const calcBy = (inputArr, sumy, sumx, lineLength) => {
    let x = 0;
    let y = 0;
    let trees = 0;

    while (x <= inputArr.length - 1 - sumx) {
        let [nx, ny] = [x + sumx, y + sumy];

        let line = inputArr[nx];
        let result = line.charAt(ny - Math.floor(ny / lineLength) * lineLength);
        trees = result === "#" ? trees + 1 : trees;
        x = x + sumx;
        y = y + sumy;
    }
    return trees;
};

(async () => {
    const inputArr = await createArr(filePath);

    let lineLength = inputArr[0].length;

    let a = calcBy(inputArr, 1, 1, lineLength);
    let b = calcBy(inputArr, 3, 1, lineLength);
    let c = calcBy(inputArr, 5, 1, lineLength);
    let d = calcBy(inputArr, 7, 1, lineLength);
    let e = calcBy(inputArr, 1, 2, lineLength);

    console.log("31. ", a * b * c * d * e);
})();
