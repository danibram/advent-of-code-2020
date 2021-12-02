const readline = require("readline");
const fs = require("fs");

let start = process.hrtime();
const [filePath] = process.argv.slice(-1);

const elapsed_time = function (note) {
    var precision = 3; // 3 decimal places
    var elapsed = process.hrtime(start)[1] / 1000000; // divide by a million to get nano to milli
    console.log(
        process.hrtime(start)[0] +
            " s, " +
            elapsed.toFixed(precision) +
            " ms - " +
            note
    ); // print message + time
    start = process.hrtime(); // reset the timer
};

const createArr = async (filePath) => {
    let rl = readline.createInterface({
        input: fs.createReadStream(filePath),
        crlfDelay: Infinity,
    });

    let numbers = [];

    for await (const line of rl) {
        numbers.push(parseInt(line));
    }

    return numbers;
};

(async () => {
    const inputArr = await createArr(filePath);

    let stop = 0;

    elapsed_time("m1-start");
    for (let i = 0; i < inputArr.length; i++) {
        for (let j = 1; j < inputArr.length - 1; j++) {
            for (let k = 2; k < inputArr.length - 2; k++) {
                if (inputArr[i] + inputArr[j] + inputArr[k] === 2020) {
                    console.log(
                        "12.",
                        inputArr[i],
                        inputArr[j],
                        inputArr[k],
                        "*=",
                        inputArr[i] * inputArr[j] * inputArr[k]
                    );
                    stop = 1;
                    break;
                }
            }
            if (stop) break;
        }
        if (stop) break;
    }
    elapsed_time("m1-end");

    const arraySorted = inputArr.sort((a, b) => a - b);
    stop = 0;
    elapsed_time("m2-start");
    for (let i = 0; i < arraySorted.length; i++) {
        for (let j = 1; j < arraySorted.length - 1; j++) {
            for (let k = 2; k < arraySorted.length - 2; k++) {
                if (arraySorted[i] + arraySorted[j] + arraySorted[k] === 2020) {
                    console.log(
                        "12.",
                        arraySorted[i],
                        arraySorted[j],
                        arraySorted[k],
                        "*=",
                        arraySorted[i] * arraySorted[j] * arraySorted[k]
                    );
                    stop = 1;
                    break;
                }
            }
            if (stop) break;
        }
        if (stop) break;
    }
    elapsed_time("m2-end");
})();
