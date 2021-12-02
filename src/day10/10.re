let parser = () => {
  let arr =
    Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0)
    |> Belt.Option.getUnsafe
    |> Node.Fs.readFileAsUtf8Sync
    |> Js.String.split("\n")
    |> Js.Array.map(v => v->String.trim->Int64.of_string);

  arr |> Array.sort((a, b) => a->Int64.sub(b)->Int64.to_int);

  arr;
};

let part1 = input => {
  let d =
    input->Belt.Array.mapWithIndex((i, x) => {
      // Js.log(
      //   string_of_int(i > 0 ? input[i - 1] : 0)
      //   ++ " "
      //   ++ string_of_int(x)
      //   ++ " "
      //   ++ string_of_int(x - (i > 0 ? input[i - 1] : 0))
      //   ++ ">",
      // );
      x->Int64.sub(i > 0 ? input[i - 1] : 0->Int64.of_int)
    });

  let threes = d->Belt.Array.keep(v => v == 3->Int64.of_int)->Array.length;
  let ones = d->Belt.Array.keep(v => v == 1->Int64.of_int)->Array.length;

  threes * ones;
};

let part2 = (input, maxI) => {
  let options = [|1, 2, 3|] |> Array.map(v => v->Int64.of_int);
  let d = Js.Dict.empty();
  d->Js.Dict.set(string_of_int(0), 1->Int64.of_int);

  input
  |> Js.Array.forEach(v => {
       d->Js.Dict.set(
         v->Int64.to_string,
         options
         |> Js.Array.reduce(
              (acc, value) => {
                // Js.log(
                //   v->Int64.to_string
                //   ++ " "
                //   ++ value->Int64.to_string
                //   ++ " "
                //   ++ acc->Int64.to_string
                //   ++ " "
                //   ++ d
                //      ->Js.Dict.get(v->Int64.sub(value)->Int64.to_string)
                //      ->Belt.Option.getWithDefault(0->Int64.of_int)
                //      ->Int64.add(acc)
                //      ->Int64.to_string
                //   ++ " >",
                // );
                d
                ->Js.Dict.get(v->Int64.sub(value)->Int64.to_string)
                ->Belt.Option.getWithDefault(0->Int64.of_int)
                ->Int64.add(acc)
              },
              0->Int64.of_int,
            ),
       )
     });

  d
  ->Js.Dict.get(maxI->Int64.to_string)
  ->Belt.Option.getWithDefault(0->Int64.of_int);
};

let input = parser();
let deviceI = input[input->Js.Array.length - 1]->Int64.add(3->Int64.of_int);
let totalInputs = input->Array.append([|deviceI|]);

Js.log("101. " ++ string_of_int(part1(totalInputs)));
Js.log("101. " ++ part2(totalInputs, deviceI)->Int64.to_string);
