type inst('item) =
  | Nop('item)
  | Acc('item)
  | Jmp('item);

type action('item) =
  | ShallNotPass('item)
  | Finished('item)
  | Continue('item);

let parser = () => {
  let instructions =
    Sys.argv->Belt.Array.sliceToEnd(-1)->Belt.Array.get(0)
    |> Belt.Option.getUnsafe
    |> Node.Fs.readFileAsUtf8Sync
    |> Js.String.split("\n");

  let parse =
    instructions
    |> Belt.Array.map(_, value => {
         switch (value |> Js.String.split(" ")) {
         | [|"jmp", value|] => Jmp(int_of_string(value))
         | [|"acc", value|] => Acc(int_of_string(value))
         | [|"nop", value|] => Nop(int_of_string(value))
         | _ => Nop(-1)
         }
       });

  parse;
};

let instructions = parser();

let pointer = ref(0);
let acc = ref(0);
let shallNotPass = (actual, next, path) => {
  let wantedDir = actual + next;
  switch (path->Js.Dict.get(string_of_int(wantedDir))) {
  | Some(true) => ShallNotPass(acc^)
  | _ =>
    path->Js.Dict.set(string_of_int(wantedDir), true);
    Continue(wantedDir);
  };
};

let jumpsNops = ref([||]);

let run = (p, path, insts) => {
  let length = insts->Js.Array.length;
  switch (p < length) {
  | true =>
    let instruction = insts[p];
    switch (instruction) {
    | Nop(_) =>
      let _ = jumpsNops^ |> Js.Array.push((p, instruction));
      shallNotPass(p, 1, path);
    | Jmp(value) =>
      let _ = jumpsNops^ |> Js.Array.push((p, instruction));
      shallNotPass(p, value, path);
    | Acc(value) =>
      acc := acc^ + value;
      shallNotPass(p, 1, path);
    };
  | _ => Finished(acc^)
  };
};

let rec part1 = (p, path) => {
  switch (run(p, path, instructions)) {
  | ShallNotPass(value) =>
    string_of_int(p + 1)
    ++ "> Loop detected (acc: "
    ++ string_of_int(value)
    ++ ")"
  | Finished(value) =>
    string_of_int(p + 1) ++ ">Finished (acc:" ++ string_of_int(value) ++ ")"
  | Continue(value) => part1(value, path)
  };
};

let runWithoutJumpsNops = (p, path, insts) => {
  let length = insts->Js.Array.length;
  switch (p < length) {
  | true =>
    let instruction = insts[p];
    switch (instruction) {
    | Nop(_) => shallNotPass(p, 1, path)
    | Jmp(value) => shallNotPass(p, value, path)
    | Acc(value) =>
      acc := acc^ + value;
      shallNotPass(p, 1, path);
    };
  | _ => Finished(acc^)
  };
};

let switchNopJmp = val_ =>
  switch (val_) {
  | Nop(value) => Jmp(value)
  | Jmp(value) => Nop(value)
  | _ => raise(Not_found)
  };

let rec part2s = (p, path, insts) => {
  switch (runWithoutJumpsNops(p, path, insts)) {
  | ShallNotPass(_) =>
    let nInsts = insts;
    let lastJumpNop =
      switch ((jumpsNops^)->Js.Array.pop) {
      | Some((val_, _)) => val_
      | _ => raise(Not_found)
      };
    nInsts[lastJumpNop] = switchNopJmp(nInsts[lastJumpNop]);
    acc := 0;
    part2s(0, Js.Dict.empty(), nInsts);
  | Finished(value) =>
    string_of_int(p + 1) ++ "> Finished (acc:" ++ string_of_int(value) ++ ")"
  | Continue(value) => part2s(value, path, insts)
  };
};

let rec part2 = (p, path, insts) => {
  switch (run(p, path, insts)) {
  | ShallNotPass(_) =>
    let nInsts = insts;
    let _ = (jumpsNops^)->Js.Array.pop;
    let lastJumpNop =
      switch ((jumpsNops^)->Js.Array.pop) {
      | Some((val_, _)) => val_
      | _ => raise(Not_found)
      };
    nInsts[lastJumpNop] = switchNopJmp(nInsts[lastJumpNop]);
    acc := 0;
    part2s(0, Js.Dict.empty(), nInsts);
  | Finished(value) =>
    string_of_int(p + 1) ++ "> Finished (acc:" ++ string_of_int(value) ++ ")"
  | Continue(value) => part2(value, path, insts)
  };
};

Js.log("81. " ++ part1(0, Js.Dict.empty()));

acc := 0;
Js.log("82. " ++ part2(0, Js.Dict.empty(), instructions));
