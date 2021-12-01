let get_line = (_: int): option(string) =>
  try(Some(input_line(stdin))) {
  | End_of_file => None
  };

let rec get_lines = (acc: list(string)) =>
  switch (input_line(stdin)) {
  | line => get_lines(acc @ [line])
  | exception End_of_file => acc
  };

let rec take = (n: int, l: list('a)): (list('a), list('a)) =>
  if (n == 0) {
    ([], l);
  } else {
    let (took, rem) = take(n - 1, List.tl(l));
    ([List.hd(l)] @ took, rem);
  };

let window = (n: int, l: list('a)) => {
  let (first, l) = take(n, l);
  let (seq, _) =
    List.fold_left(
      ((seq, acc), cur) => {
        let acc = List.tl(acc) @ [cur];
        (seq @ [acc], acc);
      },
      ([first], first),
      l,
    );
  seq;
};

let window2 = (l: list('a)) =>
  window(2, l) |> List.map(w => (List.nth(w, 0), List.nth(w, 1)));

let window3 = (l: list('a)) =>
  window(3, l)
  |> List.map(w => (List.nth(w, 0), List.nth(w, 1), List.nth(w, 2)));

Console.log("Running Test Program #1:");
let () = {
  let depths = get_lines([]) |> List.map(int_of_string);
  let answer1_pipeline = (depths: list(int)) =>
    depths
    |> window2
    |> List.map(((a, b)) => (b > a ? 1 : 0: int))
    |> List.fold_left((a, b) => a + b, 0);

  let answer1 = depths |> answer1_pipeline;
  Console.log("Part 1");
  print_endline(string_of_int(answer1));
  let answer2_pipeline = (depths: list(int)) =>
    depths
    |> window3
    |> List.map(((a, b, c)) => a + b + c)
    |> window2
    |> List.map(((a, b)) => b > a ? 1 : 0)
    |> List.fold_left((a, b) => a + b, 0);
  Console.log("Part 2");
  let answer2 = answer2_pipeline(depths);
  print_endline(string_of_int(answer2));
};
