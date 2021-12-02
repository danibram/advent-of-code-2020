let filePath = Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0);
let processArray = (acc, value) => {
  let ln = Array.length(acc);

  switch (value, ln) {
  | (_, 0) => acc->Array.append([|value|])
  | ("", _) => acc->Array.append([|""|])
  | (_, _) =>
    let lastEl = acc->Array.get(ln - 1);
    acc->Array.set(ln - 1, lastEl === "" ? value : lastEl ++ " " ++ value);
    acc;
  };
};

let arrayInput =
  Node.Fs.readFileAsUtf8Sync(Belt.Option.getUnsafe(filePath))
  |> Js.String.split("\n")
  |> Belt.Array.reduce(_, [||], processArray);

let length = arrayInput->Array.length;

Js.log({j|Read total $(length) passports |j});

let isNumber = str => {
  switch (int_of_string(str)) {
  | exception _ => false
  | _ => true
  };
};

let lengthOf = (ln, str) => {
  let ln2 = str->Js.String.length;
  ln === ln2;
};

let min = (min, n) => {
  int_of_string(n) >= min;
};

let max = (max, n) => {
  int_of_string(n) <= max;
};

let oneOf = (options, n) => {
  options->Belt.Array.reduce(false, (_acc, _val) =>
    _acc === true ? true : n == _val
  );
};

let firstChar = (first, str) => {
  str.[0] === first;
};

let hclVal = (str: string) => {
  switch (str |> firstChar('#')) {
  | true =>
    let str2 = str->String.sub(1, str->String.length - 1);
    Js.Array2.reduce(
      Js.Array2.fromMap(Js.String.castToArrayLike(str2), x => x),
      (acc, x) =>
        acc === true
          ? true
          : oneOf(
              [|
                "0",
                "1",
                "2",
                "3",
                "4",
                "5",
                "6",
                "7",
                "8",
                "9",
                "a",
                "b",
                "c",
                "d",
                "f",
              |],
              x,
            ),
      false,
    );
  | _ => false
  };
};

let hgtVal = (str: string) => {
  let fieldArr = Js.String.split("in", str);
  let fieldArr1 = Js.String.split("cm", str);

  switch (Belt.Array.length(fieldArr), Belt.Array.length(fieldArr1)) {
  | (2, _) =>
    let height = int_of_string(fieldArr[0]);
    height >= 59 && height <= 76;
  | (_, 2) =>
    let height = int_of_string(fieldArr1[0]);
    height >= 150 && height <= 193;
  | (_, _) => false
  };
};

type validations =
  | Valid
  | Invalid
  | No_Validated;

let valid = s => {
  s ? Valid : Invalid;
};

let make = (value: string, args: array(string => bool)) =>
  args->Belt.Array.reduce(true, (_acc, _val) =>
    _acc === false ? false : _val(value)
  );

let validateFields = arr => {
  switch (arr) {
  | [|key, value_|] =>
    switch (key) {
    | "byr" =>
      value_->make([|lengthOf(4), isNumber, min(1920), max(2002)|])->valid
    | "iyr" =>
      value_->make([|lengthOf(4), isNumber, min(2010), max(2020)|])->valid
    | "eyr" =>
      value_->make([|lengthOf(4), isNumber, min(2020), max(2030)|])->valid
    | "hgt" => value_->make([|hgtVal|])->valid
    | "hcl" => value_->make([|hclVal|])->valid
    | "ecl" =>
      value_
      ->make([|oneOf([|"amb", "blu", "brn", "gry", "grn", "hzl", "oth"|])|])
      ->valid
    | "pid" => value_->make([|lengthOf(9)|])->valid
    | "cid" => Valid
    | _ => No_Validated
    }
  | _ => No_Validated
  };
};

let validateAll = a => {
  let arr =
    Belt.Array.map(
      a,
      _val => {
        let fieldArr = Js.String.split(":", _val);
        [|fieldArr[0], fieldArr[1]|];
      },
    )
    ->Belt.Array.keep(a => Belt.Array.length(a) === 2);

  let must =
    arr->Belt.Array.reduce(0, (acc, _val) => {
      oneOf([|"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"|], _val[0])
        ? acc + 1 : acc
    });

  must >= 7
    ? Belt.Array.map(arr, validateFields)
      ->Belt.Array.reduce(Valid, (acc, _val) =>
          acc === Invalid ? Invalid : _val
        )
    : Invalid;
};

let correctPassports =
  arrayInput
  |> Belt.Array.map(_, fields => {
       fields |> Js.String.split(" ") |> validateAll
     })
  |> Belt.Array.keep(_, a => a === Valid)
  |> Belt.Array.size;

Js.log("42. " ++ string_of_int(correctPassports) ++ " Passports");

Js.log("42. Correct(244)");
