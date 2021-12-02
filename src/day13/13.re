let parser = () => {
  Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0)
  |> Belt.Option.getUnsafe
  |> Node.Fs.readFileAsUtf8Sync
  |> Js.String.split("\n");
};

let [|n, buses|] = parser();

let calc = (number, (index, bus)) => index->(-)(number)->( * )(bus);

let part1 = (n, buses) => {
  let num = int_of_string(n);
  buses
  |> Js.String.split(",")
  |> Belt.Array.keep(_, x => x !== "x")
  |> Belt.Array.map(_, v => int_of_string(v))
  |> Belt.Array.map(_, value =>
       (num->(/)(value)->(+)(1)->( * )(value), value)
     )
  |> Belt.Array.reduce(_, ((-1), (-1)), ((an, av), (n, value)) => {
       an === (-1) ? (n, value) : n < an ? (n, value) : (an, av)
     })
  |> calc(num);
};

let mod_ = (number: Int64.t, modNumber: Int64.t) => {
  let target = ref(number);

  while (target.contents > Int64.zero) {
    target := target.contents->Int64.sub(modNumber);
  };

  target.contents == Int64.zero;
};

let part2 = buses => {

    let dict = Js.Dict.empty()

  let break = ref(false);
  let i = ref(0->Int64.of_int);

  while (! break^) {
    i := i.contents->Int64.add(buses[0]->Int64.of_string);

    Js.log(i);

    // Js.log(
    //   buses->Belt.Array.reduceWithIndex(true, (acc, v, index) => {
    //     v === "x"
    //       ? acc
    //       : acc === false
    //           ? false
    //           : i.contents
    //             ->Int64.add(index->Int64.of_int)
    //             ->mod_(int_of_string(v)->Int64.of_int)
    //   }),
    // );

    break :=
      buses->Belt.Array.reduceWithIndex(true, (acc, v, index) => {
        v === "x"
          ? acc
          : acc === false
              ? false
              : i.contents
                ->Int64.add(index->Int64.of_int)
                ->mod_(int_of_string(v)->Int64.of_int)
      });
  };

  i.contents;
};

Js.log("13_1. " ++ string_of_int(part1(n, buses)));
Js.log("13_2. " ++ part2(buses |> Js.String.split(","))->Int64.to_string);
