let filePath = Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0);
let arrayInput =
  Node.Fs.readFileAsUtf8Sync(Belt.Option.getUnsafe(filePath))
  |> Js.String.split("\n");

let length = arrayInput->Array.length;
Js.log({j|Read total $(length) passports |j});

exception InvalidInput(string);
type direction =
  | Up
  | Down;
let castTo = str =>
  switch (str) {
  | "F" => Down
  | "B" => Up
  | "R" => Up
  | "L" => Down
  | _ => raise(InvalidInput(str))
  };

let rec next = (min: int, max: int, instruction: string) => {
  let dir = instruction->String.sub(0, 1);
  // Js.log(
  //     "min " ++ string_of_int(min) ++ ", max" ++ string_of_int(max) ++ " " ++ instruction ++ ", dir " ++ dir
  // );

  switch (castTo(dir), max - min) {
  | (Up, 1) => max
  | (Down, 1) => min
  | (Up, _) =>
    next(
      min + (max + 1 - min) / 2,
      max,
      instruction->String.sub(1, String.length(instruction) - 1),
    )
  | (Down, _) =>
    next(
      min,
      min + (max + 1 - min) / 2 - 1,
      instruction->String.sub(1, String.length(instruction) - 1),
    )
  };
};

let part1 = (arrayInput) => {
    arrayInput->Belt.Array.reduce(
        0,
        (acc, _val) => {
          let row = _val |> Js.String.slice(~from=0, ~to_=7);
          let col = _val |> Js.String.slice(~from=7, ~to_=10);

          let result = 8 * next(0, 127, row) + next(0, 7, col);

        //   Js.log(_val ++ ": " ++ string_of_int(result));

          result > acc ? result : acc;
        },
      )
}

let part2 = (arrayInput) => {
    let seats =
    arrayInput->Belt.Array.map(_val => {
        let row = _val |> Js.String.slice(~from=0, ~to_=7);
        let col = _val |> Js.String.slice(~from=7, ~to_=10);
        8 * next(0, 127, row) + next(0, 7, col);
    });

    Array.sort((a, b) => a - b, seats);

    let min = seats[0];
    let value = ref(min + 1);
    let break = ref(false);
    let i = ref(0);
    let res = ref(0);
    let results = ref([|0|]);

    while (! break^) {
        i := i^ + 1;
        value := value^;

        if (value^ + 1 !== seats[i^ + 1]) {
            value := value^ + 1;
            // Js.log(value);
            break := true;
        } else {
            value := value^ + 1;
        };
    };

    value.contents;
}

Js.log("5_1. " ++ string_of_int(part1(arrayInput)));
Js.log("5_2. " ++ string_of_int(part2(arrayInput)));
