let cleanColor = str =>
  str |> Js.String.splitAtMost(" bag", ~limit=1) |> Js.Array.unsafe_get(_, 0);

let parser = () => {
  let res =
    Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0)
    |> Belt.Option.getUnsafe
    |> Node.Fs.readFileAsUtf8Sync
    |> Js.String.split("\n")
    |> Belt.Array.reduce(
         _,
         Js.Dict.empty(),
         (acc, value) => {
           switch (value |> Js.String.splitAtMost(" bags contain ", ~limit=2)) {
           | [|key, "no other bags."|] =>
             acc->Js.Dict.set(key, Js.Dict.empty())
           | [|key, cnt|] =>
             let child = Js.Dict.empty();
             cnt
             |> Js.String.split(", ")
             |> Js.Array.forEach(_v => {
                  child->Js.Dict.set(
                    _v |> Js.String.substringToEnd(~from=2) |> cleanColor,
                    _v
                    |> Js.String.substring(~from=0, ~to_=1)
                    |> int_of_string,
                  )
                });

             Js.Dict.set(acc, key, child);
           | _ => ()
           };

           acc;
         },
       );

  res;
};

let res = parser();

//Part1
let rec finder = (dict, shinyCache) => {
  let startShinies = shinyCache->Js.Dict.keys;

  dict
  ->Js.Dict.entries
  ->Belt.Array.forEach(((color, bags)) => {
      bags
      ->Js.Dict.entries
      ->Belt.Array.forEach(((k, _)) => {
          switch (k) {
          | "shiny gold" => shinyCache->Js.Dict.set(color, 1)
          | _ =>
            switch (startShinies |> Js.Array.includes(k)) {
            | true => shinyCache->Js.Dict.set(color, 1)
            | _ => ()
            }
          }
        })
    });

  if (startShinies->Array.length === shinyCache->Js.Dict.keys->Array.length) {
    shinyCache->Js.Dict.keys->Array.length;
  } else {
    finder(dict, shinyCache);
  };
};

//Part2
let rec calcCost = (bags, color) => {
  let total = ref(0);

  switch (bags->Js.Dict.get(color)) {
  | Some(bag) =>
    bag
    ->Js.Dict.entries
    ->Js.Array.forEach(
        ((k, v)) => {
          let c = v;
          total := total^ + c;
          total := total^ + c * calcCost(bags, k);
        },
        _,
      );

    total^;
  | None => total^
  };
};

Js.log("71. " ++ string_of_int(finder(res, Js.Dict.empty())));
Js.log("72. " ++ string_of_int(calcCost(res, "shiny gold")));
