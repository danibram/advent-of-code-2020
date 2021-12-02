let filePath = Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0);
let arrayInput =
  Node.Fs.readFileAsUtf8Sync(Belt.Option.getUnsafe(filePath))
  |> Js.String.split("\n");

let processArray = (acc, value) => {
  let ln = Array.length(acc);

  switch (value, ln) {
  | (_, 0) => acc->Array.append([|[|value|]|])
  | ("", _) => acc->Array.append([|[||]|])
  | (_, _) =>
    let lastEl = acc->Array.get(ln - 1);
    acc->Array.set(ln - 1, lastEl->Array.append([|value|]));
    acc;
  };
};

let processArray2 = (acc, value) => {
  let ln = Array.length(acc);

  switch (value, ln) {
  | (_, 0) => acc->Array.append([|value|])
  | ("", _) => acc->Array.append([|""|])
  | (_, _) =>
    let lastEl = acc->Array.get(ln - 1);
    acc->Array.set(ln - 1, lastEl === "" ? value : lastEl ++ value);
    acc;
  };
};

let part1 = arrayInput => {
  arrayInput
  ->Belt.Array.reduce(_, [||], processArray2)
  ->Belt.Array.map(_val => {
      let d = Js.Dict.empty();
      _val
      |> String.iter(_v => {
           let key = String.make(1, _v);
           let newCount = d->Js.Dict.get(key)->Belt.Option.getWithDefault(1);
           Js.Dict.set(d, key, newCount);
         });

      d->Js.Dict.values->Array.fold_left((acc, _v) => {acc + _v}, 0, _);
    })
  ->Array.fold_left((acc, _v) => {acc + _v}, 0, _);
};

let part2 = arrayInput => {
  arrayInput
  ->Belt.Array.reduce(_, [||], processArray)
  ->Belt.Array.map(_val => {
      let d = Js.Dict.empty();
      let ln = _val->Array.length;
      _val->Belt.Array.forEach(_vs => {
        _vs->String.iter(
               _v => {
                 let key = String.make(1, _v);
                 let newCount =
                   d
                   ->Js.Dict.get(key)
                   ->Belt.Option.getWithDefault(0)
                   ->(+)(1);
                 Js.Dict.set(d, key, newCount);
               },
               _,
             )
      });

      d
      ->Js.Dict.entries
      ->Belt.Array.keep(((_, _v)) => _v === ln)
      ->Belt.Array.map(_, ((_, _v)) => _v / ln)
      ->Array.fold_left((acc, _v) => {acc + _v}, 0, _);
    })
  ->Array.fold_left((acc, _v) => {acc + _v}, 0, _);
};

Js.log("6_1. " ++ string_of_int(part1(arrayInput)));
Js.log("6_2. " ++ string_of_int(part2(arrayInput)));
