module Command = {
  type t =
    | Forward(int)
    | Up(int)
    | Down(int);
  let parse = (s: string): result(t, string) => {
    let args = Lib.StringUtil.fields(s);
    switch (args) {
    | [op, arg] =>
      switch (int_of_string_opt(arg)) {
      | Some(v) =>
        switch (op) {
        | "forward" => Ok(Forward(v))
        | "up" => Ok(Up(v))
        | "down" => Ok(Down(v))
        | s => Error(Printf.sprintf("invalid command: %S", s))
        }
      | None =>
        Error(
          Printf.sprintf(
            "invalid command argument: %S is not an integer",
            arg,
          ),
        )
      }
    | _ =>
      Error(
        Printf.sprintf(
          "invalid command: want %d fields, got %d",
          2,
          List.length(args),
        ),
      )
    };
  };
};

type sub_state = {
  aim: int,
  pos: (int, int),
};

Console.log("Running Test Program #2:");
let () = {
  let commands =
    stdin
    |> Lib.StreamUtil.lines
    |> Lib.StreamUtil.filter_map(line =>
         switch (String.trim(line)) {
         | "" => None
         | s => Some(s)
         }
       )
    |> Lib.StreamUtil.map(Command.parse)
    |> Lib.StreamUtil.fold_left(
         (state, pr) =>
           switch (state, pr) {
           | (Ok(acc), Ok(cmd)) => Ok(acc @ [cmd])
           | (Error(msg), _)
           | (_, Error(msg)) => Error(msg)
           },
         Ok([]),
       )
    |> (
      result =>
        switch (result) {
        | Ok(acc) => acc
        | Error(msg) =>
          Console.error(msg);
          exit(1);
        }
    );
  Console.log("Part 1:");
  let part1_answer =
    Stream.of_list(commands)
    |> Lib.StreamUtil.fold_left(
         ((x, y), cmd) =>
           switch (cmd) {
           | Command.Forward(v) => (x + v, y)
           | Command.Up(v) => (x, y - v)
           | Command.Down(v) => (x, y + v)
           },
         (0, 0),
       )
    |> (((dx, dy)) => dx * dy);
  Console.log(part1_answer);
  Console.log("Part 2:");
  let part2_answer =
    Stream.of_list(commands)
    |> Lib.StreamUtil.fold_left(
         ({aim, pos: (x, y)}, cmd) =>
           switch (cmd) {
           | Command.Forward(v) => {aim, pos: (x + v, y + aim * v)}
           | Command.Up(v) => {aim: aim - v, pos: (x, y)}
           | Command.Down(v) => {aim: aim + v, pos: (x, y)}
           },
         {aim: 0, pos: (0, 0)},
       )
    |> (({pos: (dx, dy), _}) => dx * dy);
  Console.log(part2_answer);
};
