Console.log("Running Test Program #3:");
let () = {
  let s = Lib.Stream2.batch(3, Stream.of_list([1, 2, 3, 4, 5, 6, 7, 8]));
  Stream.iter(x => Console.log(x), s);
};
