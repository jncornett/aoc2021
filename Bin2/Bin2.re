let get_line = (_: int): option(string) =>
  try(Some(input_line(stdin))) {
  | End_of_file => None
  };

let rec get_lines = (acc: list(string)) =>
  switch (input_line(stdin)) {
  | line => get_lines(acc @ [line])
  | exception End_of_file => acc
  };

Console.log("Running Test Program #1:");
let () = {
  let depths = get_lines([]) |> List.map(int_of_string);
  let increases =
    List.map2(
      (a, b) => b > a ? 1 : 0,
      depths,
      List.concat([List.tl(depths), [0]]),
    );
  let sum = List.fold_left((a, b) => a + b, 0, increases);
  print_endline(string_of_int(sum));
};
