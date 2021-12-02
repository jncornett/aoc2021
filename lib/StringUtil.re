let index_any = (chars: list(char), s: string) =>
  List.find_map(ch =>
    try(Some(String.index(s, ch))) {
    | Not_found => None
    }
  );

let default_field_separators: list(char) = [' ', '\t'];

let rec fields = (s: string) => {
  let out = ref([]);
  let cur = ref(0);
  while (cur^ < String.length(s)) {
    let next_sep_index =
      default_field_separators
      |> List.find_map(ch => ch |> String.index_from_opt(s, cur^));
    switch (next_sep_index) {
    | None =>
      out := out^ @ [String.sub(s, cur^, String.length(s) - cur^)];
      cur := String.length(s);
    | Some(i) =>
      switch (i) {
      | 0 => cur := cur^ + 1
      | i =>
        out := out^ @ [String.sub(s, cur^, i - cur^)];
        cur := i + 1;
      }
    };
  };
  out^;
};
