type take_result('a) = result(list('a), list('a));

let rec drop = (n: int, s: Stream.t('a)) =>
  if (n <= 0) {
    ();
  } else {
    switch (Stream.next(s)) {
    | exception Stream.Failure => ()
    | _ => drop(n - 1, s)
    };
  };

let take = (n: int, s: Stream.t('a)) => {
  let acc = Stream.npeek(n, s);
  drop(List.length(acc), s);
  if (List.length(acc) < n) {
    Error(acc);
  } else {
    Ok(acc);
  };
};

let map = (fn: 'a => 'b, s: Stream.t('a)) =>
  Stream.from(_ =>
    switch (Stream.next(s)) {
    | v => Some(fn(v))
    | exception Stream.Failure => None
    }
  );

let window = (n: int, s: Stream.t('a)) =>
  switch (take(n, s)) {
  | Ok(initial) =>
    let frame = ref(initial);
    Stream.from(i =>
      if (i == 0) {
        Some(frame^);
      } else {
        switch (Stream.next(s)) {
        | v =>
          frame := List.tl(frame^) @ [v];
          Some(frame^);
        | exception Stream.Failure => None
        };
      }
    );
  | Error(_) => Stream.sempty
  };

let window2 = s =>
  s |> window(2) |> map(frame => (List.nth(frame, 0), List.nth(frame, 1)));

let window3 = s =>
  s
  |> window(3)
  |> map(frame =>
       (List.nth(frame, 0), List.nth(frame, 1), List.nth(frame, 2))
     );

let fold_left = (fn: ('b, 'a) => 'b, init: 'b, s: Stream.t('a)) => {
  let state = ref(init);
  Stream.iter(v => {state := fn(state^, v)}, s);
  state^;
};

let lines = (chan: in_channel) =>
  Stream.from(_ =>
    try(Some(input_line(chan))) {
    | End_of_file => None
    }
  );
