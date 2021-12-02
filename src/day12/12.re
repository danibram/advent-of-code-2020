let parser = () => {
  Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0)
  |> Belt.Option.getUnsafe
  |> Node.Fs.readFileAsUtf8Sync
  |> Js.String.split("\n");
};

type moveDirection('item) =
  | N('item)
  | S('item)
  | W('item)
  | E('item)
  | F('item)
  | R('item)
  | L('item);

exception CantParseInstruction(string);
exception CantParseDegrees(int);
let parseInstruction = instruction =>
  switch (
    instruction |> Js.String.substring(~from=0, ~to_=1),
    instruction |> Js.String.substringToEnd(~from=1),
  ) {
  | ("R", value) => R(value)
  | ("L", value) => L(value)
  | ("F", value) => F(value)
  | ("N", value) => N(value)
  | ("S", value) => S(value)
  | ("W", value) => W(value)
  | ("E", value) => E(value)
  | (_, _) => raise(CantParseInstruction(instruction))
  };

let addDegrees = (a, b) => {
  let res = a + b;
  res === 360 ? 0 : res > 360 ? res - 360 : res < 0 ? res + 360 : res;
};

let goPart1 = (value, direction) =>
  switch (parseInstruction(value)) {
  | N(v) => (0, int_of_string(v), direction)
  | S(v) => (0, - int_of_string(v), direction)
  | E(v) => (int_of_string(v), 0, direction)
  | W(v) => (- int_of_string(v), 0, direction)
  | R(v) => (0, 0, addDegrees(direction, - int_of_string(v)))
  | L(v) => (0, 0, addDegrees(direction, int_of_string(v)))
  | F(d) =>
    direction === 90
      ? (0, int_of_string(d), direction)
      : direction === 270
          ? (0, - int_of_string(d), direction)
          : direction === 0
              ? (int_of_string(d), 0, direction)
              : direction === 180
                  ? (- int_of_string(d), 0, direction)
                  : raise(CantParseDegrees(direction))
  };

let part1 = input => {
  let (dx, dy, _) =
    input
    |> Js.Array.reduce(
         ((x, y, direction), value) => {
           let (dx, dy, dd) = goPart1(value, direction);
           (x + dx, y + dy, dd);
         },
         (0, 0, 0),
       );

  dx->Js.Math.abs_int->(+)(dy->Js.Math.abs_int);
};

let goPart2 = (value, (x, y), (wx, wy)) =>
  switch (parseInstruction(value)) {
  | N(v) => ((x, y), (wx, wy + int_of_string(v)))
  | S(v) => ((x, y), (wx, wy - int_of_string(v)))
  | E(v) => ((x, y), (wx + int_of_string(v), wy))
  | W(v) => ((x, y), (wx - int_of_string(v), wy))
  | L(v) =>
    let rx = wx - x;
    let ry = wy - y;
    v === "90"
      ? ((x, y), (x - ry, y + rx))
      : v === "270"
          ? ((x, y), (x + ry, y - rx))
          : v === "0"
              ? ((x, y), (x + rx, y + ry))
              : v === "180"
                  ? ((x, y), (x - rx, y - ry))
                  : raise(CantParseInstruction(value));
  | R(v) =>
    let rx = wx - x;
    let ry = wy - y;

    v === "90"
      ? ((x, y), (x + ry, y - rx))
      : v === "270"
          ? ((x, y), (x - ry, y + rx))
          : v === "0"
              ? ((x, y), (x + rx, y + ry))
              : v === "180"
                  ? ((x, y), (x - rx, y - ry))
                  : raise(CantParseInstruction(value));
  | F(d) =>
    let mx = (wx - x) * int_of_string(d);
    let my = (wy - y) * int_of_string(d);

    ((x + mx, y + my), (wx + mx, wy + my));
  };

let part2 = input => {
  let (dx, dy, _, _) =
    input
    |> Js.Array.reduce(
         ((x, y, wx, wy), value) => {
           //    Js.log(value);
           //    Js.log((x, y, wx, wy));
           let ((x, y), (wdx, wdy)) = goPart2(value, (x, y), (wx, wy));
           //    Js.log("<>");
           //    Js.log((x, y, wdx, wdy));
           (x, y, wdx, wdy);
         },
         (0, 0, 10, 1),
       );

  dx->Js.Math.abs_int->(+)(dy->Js.Math.abs_int);
};

let input = parser();

Js.log("12_1. " ++ string_of_int(part1(input)));
Js.log("12_2. " ++ string_of_int(part2(input)));
