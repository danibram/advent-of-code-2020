let parser = () => {
  Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0)
  |> Belt.Option.getUnsafe
  |> Node.Fs.readFileAsUtf8Sync
  |> Js.String.split("\n")
  |> Js.Array.map(v => v |> Js.String.split(""));
};

type directions =
  | Up
  | Down
  | Left
  | Right
  | UpLeft
  | UpRight
  | DownRight
  | DownLeft;

let getPostion = value =>
  switch (value) {
  | Up => (1, 0)
  | Down => ((-1), 0)
  | Right => (0, 1)
  | Left => (0, (-1))
  | UpRight => (1, 1)
  | UpLeft => (1, (-1))
  | DownRight => ((-1), 1)
  | DownLeft => ((-1), (-1))
  };

let lookArround = [|
  Up,
  Down,
  Left,
  Right,
  UpLeft,
  UpRight,
  DownRight,
  DownLeft,
|];

let loopMatrix = (input, fn) => {
  for (i in 0 to input->Js.Array.length - 1) {
    for (j in 0 to input[0]->Js.Array.length - 1) {
      fn(input[i][j], i, j);
    };
  };
};

let siteAdjacents = (input, x, y, cmp) => {
  lookArround->Belt.Array.reduce(
    (0, 0),
    ((a,b), val_) => {
      let (dx, dy) = getPostion(val_);
      switch (input[x + dx][y + dy]) {
      | exception _ => (a,b)
      | "." => (a,b)
      | value =>
        value === cmp ? (a + 1, b + 1) : (a, b + 1);
      };
    },
  );
};

let processByAdjacent = input => {
  let output = input |> Js.Array.map(val_ => val_ |> Js.Array.map(v => v));

  input->loopMatrix((value, i, j) => {
    switch (value) {
    | "." => ()
    | "L" =>
      let (a, b) = siteAdjacents(input, i, j, "L");
      a == b ? output[i][j] = "#" : ();
    | "#" =>
      let (a, _) = siteAdjacents(input, i, j, "#");
      a >= 4 ? output[i][j] = "L" : ();
    | _ => ()
    }
  });

  output;
};

let haveChanges = (input, output) => {
  let changes = ref(0);
  input->loopMatrix((value, i, j) => {
    value === output[i][j] ? () : changes := changes^ + 1
  });
  changes.contents;
};

let countAll = (input, cmp) => {
  let changes = ref(0);
  input->loopMatrix((value, _, _) => {
    value == cmp ? changes := changes^ + 1 : ()
  });
  changes.contents;
};

let printMatrix = input => {
  input->Belt.Array.forEach(columns =>
    Js.log(columns |> Js.Array.joinWith(""))
  );
};

let rec part1 = (lastDiff, input) => {
  let output = processByAdjacent(input);

  let diffs = haveChanges(input, output);

  switch (lastDiff !== diffs) {
  | true => part1(diffs, output)
  | _ => countAll(input, "#")
  };
};

let rec findNext = (direction, i, j, input) => {
  let (dx, dy) = getPostion(direction);

  switch (input[i + dx][j + dy]) {
  | exception _ => "."
  | "." =>
    let (dx, dy) = getPostion(direction);
    findNext(direction, i + dx, j + dy, input);
  | value => value
  };
};

let nextSeat = (input, x, y, cmp) => {
  lookArround->Belt.Array.reduce((0, 0), ((a, b), val_) => {
    switch (findNext(val_, x, y, input)) {
    | exception _ => (a, b)
    | "." => (a, b)
    | value => value === cmp ? (a + 1, b + 1) : (a, b + 1)
    }
  });
};

let processByNext = input => {
  let output = input |> Js.Array.map(val_ => val_ |> Js.Array.map(v => v));

  input->loopMatrix((value, i, j) => {
    switch (value) {
    | "." => ()
    | "L" =>
      let (a, b) = nextSeat(input, i, j, "L");
      a == b ? output[i][j] = "#" : ();
    | "#" =>
      let (a, _) = nextSeat(input, i, j, "#");
      a >= 5 ? output[i][j] = "L" : ();
    | _ => ()
    }
  });

  output;
};

let rec part2 = (lastDiff, input) => {
  let output = processByNext(input);

  let diffs = haveChanges(input, output);

  switch (lastDiff !== diffs) {
  | true => part2(diffs, output)
  | _ => countAll(input, "#")
  };
};

let input = parser();

Js.log("11_1. " ++ string_of_int(part1(-1, input)));
Js.log("11_2. " ++ string_of_int(part2(-1, input)));
