include Lib.Prelude;

let silver = depths =>
  depths
  |> Stream2.window2
  |> Stream2.map(((a, b)) => b > a ? 1 : 0)
  |> Stream2.fold_left((a, b) => a + b, 0);

let gold = depths =>
  depths
  |> Stream2.window3
  |> Stream2.map(((a, b, c)) => a + b + c)
  |> Stream2.window2
  |> Stream2.map(((a, b)) => b > a ? 1 : 0)
  |> Stream2.fold_left((a, b) => a + b, 0);

Console.log("Running Test Program #1:");
let () = {
  let depths =
    Io.lines(stdin) |> Stream2.map(int_of_string) |> Stream2.to_list;
  Console.log("Silver:");
  Console.log(string_of_int(silver(Stream.of_list(depths))));
  Console.log("Gold:");
  Console.log(string_of_int(gold(Stream.of_list(depths))));
};
