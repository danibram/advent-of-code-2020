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

const calcBy = (inputArr, sumx, sumy, lineLength) => {
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

    let x = 0;
    let y = 0;
    let trees = 0;

    let lineLength = inputArr[0].length;

    while (x <= inputArr.length - 2) {
        let [nx, ny] = [x + 1, y + 3];

        let line = inputArr[nx];
        let result = line.charAt(ny - Math.floor(ny / lineLength) * lineLength);
        trees = result === "#" ? trees + 1 : trees;
        x = x + 1;
        y = y + 3;
    }

    console.log("31. ", calcBy(inputArr, 1, 3, lineLength));
})();
