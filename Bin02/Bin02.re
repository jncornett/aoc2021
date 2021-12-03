include Lib.Prelude;

type command =
  | Forward(int)
  | Up(int)
  | Down(int);

let reduce_silver = ((dx, dy): (int, int), cmd: command) =>
  switch (cmd) {
  | Forward(v) => (dx + v, dy)
  | Up(v) => (dx, dy - v)
  | Down(v) => (dx, dy + v)
  };

let reduce_gold = ((aim, (dx, dy)): (int, (int, int)), cmd: command) =>
  switch (cmd) {
  | Forward(v) => (aim, (dx + v, dy + aim * v))
  | Up(v) => (aim - v, (dx, dy))
  | Down(v) => (aim + v, (dx, dy))
  };

exception ParseError(string);

let parse_command = (s: string) =>
  Scanf.sscanf(s, "%s %d", (op, v) =>
    switch (op) {
    | "forward" => Forward(v)
    | "up" => Up(v)
    | "down" => Down(v)
    | _ => raise(ParseError(Printf.sprintf("invalid command: %S", op)))
    }
  );

Console.log("Running Test Program #2:");
let () = {
  let commands =
    Io.lines(stdin)
    |> Stream2.map(String.trim)
    |> Stream2.filter(s => String.length(s) > 0)
    |> Stream2.map(parse_command)
    |> Stream2.to_list;
  Console.log("Silver:");
  Stream.of_list(commands)
  |> Stream2.fold_left(reduce_silver, (0, 0))
  |> (((dx, dy)) => dx * dy)
  |> string_of_int
  |> Console.log;
  Console.log("Gold:");
  Stream.of_list(commands)
  |> Stream2.fold_left(reduce_gold, (0, (0, 0)))
  |> (((_, (dx, dy))) => dx * dy)
  |> string_of_int
  |> Console.log;
};
