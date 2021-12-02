Console.log("Running Test Program #1:");
let () = {
  let depths =
    Lib.StreamUtil.lines(stdin) |> Lib.StreamUtil.map(int_of_string);
  let answer1_pipeline = (depths: Stream.t(int)) =>
    depths
    |> Lib.StreamUtil.window2
    |> Lib.StreamUtil.map(((a, b)) => b > a ? 1 : 0)
    |> Lib.StreamUtil.fold_left((a, b) => a + b, 0);
  let answer1 = depths |> answer1_pipeline;
  Console.log("Part 1");
  print_endline(string_of_int(answer1));
  let answer2_pipeline = (depths: Stream.t(int)) =>
    depths
    |> Lib.StreamUtil.window3
    |> Lib.StreamUtil.map(((a, b, c)) => a + b + c)
    |> Lib.StreamUtil.window2
    |> Lib.StreamUtil.map(((a, b)) => b > a ? 1 : 0)
    |> Lib.StreamUtil.fold_left((a, b) => a + b, 0);
  Console.log("Part 2");
  let answer2 = answer2_pipeline(depths);
  print_endline(string_of_int(answer2));
};
