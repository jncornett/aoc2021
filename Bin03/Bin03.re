include Lib.Prelude;

module Data = {
  type row = array(bool);
  type frame = array(row);

  let empty = (): frame => Array.init(0, _ => Array.make(0, false));

  let parse_row = (s: string): result(row, string) =>
    Stream.of_string(s)
    |> Stream2.map(ch =>
         switch (ch) {
         | '0' => Ok(false)
         | '1' => Ok(true)
         | _ => Error("invalid character")
         }
       )
    |> Stream2.fold_left_result((acc, x) => acc @ [x], [])
    |> Result.map(Array.of_list);

  let parse = (lines: stream(string)) =>
    lines
    |> Stream2.filter_map(line =>
         switch (String.trim(line)) {
         | "" => None
         | trimmed => Some(trimmed)
         }
       )
    |> Stream2.map(parse_row)
    |> Stream2.fold_left_result((acc, row) => acc @ [row], [])
    |> Result.map(Array.of_list);

  let width = (fr: frame) =>
    Array.length(fr) == 0 ? None : Some(Array.length(fr[0]));

  let transpose = (fr: frame) =>
    switch (width(fr)) {
    | Some(w) =>
      Array.init(w, i => Array.init(Array.length(fr), j => fr[j][i]))
    | None => empty()
    };

  let to_int = (rw: row) =>
    Array.to_seq(rw)
    |> Seq.map(b => b ? '1' : '0')
    |> String.of_seq
    |> (++)("0b")
    |> int_of_string;
};

let compute_bias_i = (i: int, fr: Data.frame) =>
  fr
  |> Array.map(rw => rw[i] ? 1 : (-1))
  |> Array.fold_left((acc, x) => acc + x, 0);

let compute_biases_all = fr =>
  Data.transpose(fr)
  |> Array.map(row =>
       row
       |> Array.map(b => b ? 1 : (-1))
       |> Array.fold_left((acc, x) => acc + x, 0)
     );

let xor = (a, b) =>
  switch (a, b) {
  | (false, false) => false
  | (true, true) => false
  | _ => true
  };
let rec sieve = (invert: bool, i: int, n: int, rows: Data.frame) =>
  switch (rows) {
  | [||] => [||]
  | [|rw|] => [|rw|]
  | rows =>
    i == n
      ? rows
      : {
        let cmp = xor(compute_bias_i(i, rows) >= 0, invert);
        let next_rows =
          Array.to_list(rows)
          |> List.filter(rw => rw[i] == cmp)
          |> Array.of_list;
        sieve(invert, i + 1, n, next_rows);
      }
  };

Console.log("Running Test Program #3:");
let () = {
  let frame = Io.lines(stdin) |> Data.parse |> Result.get_ok;
  let biases = compute_biases_all(frame);
  let gamma =
    biases
    |> Array.mapi((i, bias) =>
         switch (bias) {
         | bias when bias < 0 => Ok(false)
         | bias when bias > 0 => Ok(true)
         | _ => Error("no bias for position " ++ string_of_int(i))
         }
       )
    |> Array.map(Result.get_ok);
  let epsilon =
    biases
    |> Array.mapi((i, bias) =>
         switch (bias) {
         | bias when bias < 0 => Ok(true)
         | bias when bias > 0 => Ok(false)
         | _ => Error("no bias for position " ++ string_of_int(i))
         }
       )
    |> Array.map(Result.get_ok);
  Console.log("Silver:");
  Console.log(Data.to_int(gamma) * Data.to_int(epsilon));
  let oxy = sieve(false, 0, Data.width(frame) |> Option.get, frame);
  let co2 = sieve(true, 0, Data.width(frame) |> Option.get, frame);
  Console.log("Gold:");
  Console.log(Data.to_int(oxy[0]) * Data.to_int(co2[0]));
};
