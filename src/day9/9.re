let parser = () => {
  Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0)
  |> Belt.Option.getUnsafe
  |> Node.Fs.readFileAsUtf8Sync
  |> Js.String.split("\n")
  |> Js.Array.map(v => String.trim(v));
};

let initDB = input => {
  let d = Js.Dict.empty();
  let inputLength = input->Js.Array.length;

  for (i in 0 to inputLength - 1) {
    for (j in 0 to inputLength - 1) {
      let a = input[i];
      let b = input[j];
      if (a !== b) {
        let aa = Belt.Int.fromString(a)->Belt.Option.getWithDefault(0);
        let bb = Belt.Int.fromString(b)->Belt.Option.getWithDefault(0);
        Js.Dict.set(d, Belt.Int.toString(aa + bb), (a, b));
      };
    };
  };
  d;
};

let rec part1 = (~i=0, input, ln) => {
  let selected = input |> Js.Array.slice(~start=i, ~end_=ln + i);

  let db = initDB(selected);
  let checkingN = input[ln + i];

  switch (db->Js.Dict.get(checkingN)) {
  | Some(_) => part1(input, ln, ~i=i + 1)
  | None => checkingN
  };
};

let input = parser();
let selectedNumber = part1(input, 25);

Js.log("91. " ++ selectedNumber);

type ops =
  | Next
  | Matched
  | Continue;

let decissor = n =>
  switch (n == 0, n > 0) {
  | (true, _) => Matched
  | (_, true) => Continue
  | (_, _) => Next
  };

let rec secondLoop = (~i=0, acc, number, input) => {
  let target = int_of_string(input[i]);

  let res = number - target;

  switch (res->decissor, i < 2) {
  | (_, true) =>
    secondLoop(acc->Js.Array.concat([|target|]), res, input, ~i=i + 1)
  | (Matched, _) => acc->Js.Array.concat([|target|])
  | (Continue, _) =>
    secondLoop(acc->Js.Array.concat([|target|]), res, input, ~i=i + 1)
  | (Next, _) => [||]
  };
};

let rec firstLoop = (~i=0, number, input) => {
  switch (secondLoop([||], number, input, ~i)) {
  | [||] => firstLoop(~i=i + 1, number, input)
  | val_ => val_
  };
};

let part2 = (number, input) => {
  let arr = firstLoop(number, input);
  arr |> Array.sort((a, b) => a - b);

  arr[0] + arr[arr->Array.length - 1];
};

Js.log(
  "92. "
  ++ string_of_int(
       part2(
         Belt.Int.fromString(selectedNumber)->Belt.Option.getWithDefault(0),
         input,
       ),
     ),
);
