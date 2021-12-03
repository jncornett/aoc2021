include Lib.Prelude;

Console.log("Running Test Program #4:");

let s: stream(result(int, string)) =
  Stream.of_list([Ok(1), Ok(2), Ok(3), Error("oh"), Ok(4)]);
let a =
  Stream2.fold_left_result(
    (acc: list(int), x: int): list(int) => acc @ [x],
    [],
    s,
  );
Console.log(a);
let () =
  switch (a) {
  | Ok(out) => Console.log(out)
  | Error(err) => Console.error(err)
  };
