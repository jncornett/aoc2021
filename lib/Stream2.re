type stream('a) = Stream.t('a);

let next = (s: stream('a)) =>
  try(Some(Stream.next(s))) {
  | Stream.Failure => None
  };

module Detail = {
  let stateful = (fn: 'state => ('state, option('item)), init: 'state) => {
    let state = ref(init);
    Stream.from(_ => {
      let (next_state, x) = fn(state^);
      state := next_state;
      x;
    });
  };

  let lazy_memo = (fn: unit => 'a) => {
    let cache = ref(None);
    () =>
      switch (cache^) {
      | Some(v) => v
      | None =>
        let v = fn();
        cache := Some(v);
        v;
      };
  };

  let filter_next = (fn: 'a => bool, s: stream('a)) =>
    Util.loop_until(
      s =>
        switch (next(s)) {
        | Some(x) => fn(x) ? Error(Some(x)) : Ok(s)
        | None => Error(None)
        },
      s,
    );

  let filter_map_next = (fn: 'a => option('b), s: stream('a)) =>
    Util.loop_until(
      s =>
        switch (next(s)) {
        | Some(x) =>
          switch (fn(x)) {
          | Some(y) => Error(Some(y))
          | None => Ok(s)
          }
        | None => Error(None)
        },
      s,
    );

  let rec flat_next = (ss: stream(stream('a))) =>
    Util.loop_until(
      ss =>
        switch (Stream.peek(ss)) {
        | Some(s) =>
          switch (next(s)) {
          | Some(x) => Error(Some(x))
          | None =>
            let _ = next(ss); // flush peeked
            Ok(ss);
          }
        | None => Error(None)
        },
      ss,
    );
};

let fold_left = (fn: ('b, 'a) => 'b, init: 'b, s: stream('a)) => {
  let acc = ref(init);
  s |> Stream.iter(x => acc := fn(acc^, x));
  acc^;
};

let filter = (fn: 'a => bool, s: stream('a)) =>
  Stream.from(_ => Detail.filter_next(fn, s));

let filter_map = (fn: 'a => option('b), s: stream('a)) =>
  Stream.from(_ => Detail.filter_map_next(fn, s));

let to_list = (s: stream('a)) => fold_left((acc, x) => acc @ [x], [], s);

let map = (fn: 'a => 'b, s: stream('a)) =>
  Stream.from(_ => Option.map(fn, next(s)));

let mapi = (fn: (int, 'a) => 'b, s: stream('a)) =>
  Stream.from(i => Option.map(x => fn(i, x), next(s)));

let find = (fn: 'a => bool, s: stream('a)) => next(filter(fn, s));

let find_map = (fn: 'a => option('b), s: stream('a)) =>
  next(filter_map(fn, s));

let take = (n: int, s: stream('a)) =>
  Stream.from(i => i < n ? next(s) : None);

let rec drop = (n: int, s: stream('a)) =>
  n <= 0
    ? s
    : (
      switch (next(s)) {
      | Some(_) => drop(n, s)
      | None => Stream.sempty
      }
    );

let consume = (s: stream('a)) => Stream.iter(_ => (), s);

let flatten = (ss: stream(stream('a))) =>
  Stream.from(_ => Detail.flat_next(ss));

let take_while = (fn: 'a => bool, s: stream('a)) =>
  Stream.from(_ =>
    switch (next(s)) {
    | Some(x) => fn(x) ? Some(x) : None
    | None => None
    }
  );

let drop_while = (fn: 'a => bool, s: stream('a)) => {
  Detail.stateful(
    drop =>
      (
        false,
        drop
          ? Util.loop_until(
              s =>
                switch (next(s)) {
                | Some(x) => fn(x) ? Ok(s) : Error(Some(x))
                | None => Error(None)
                },
              s,
            )
          : next(s),
      ),
    true,
  );
};

let zip_shortest = (sl: list(stream('a))) =>
  Stream.from(_ => {
    let frame = List.map(next, sl);
    List.exists(Option.is_none, frame)
      ? None : Some(List.map(Option.get, frame));
  });

let zip_longest = (sl: list(stream('a))) =>
  Stream.from(_ => {
    let frame = List.map(next, sl);
    List.for_all(Option.is_none, frame) ? None : Some(frame);
  });

let window = (n: int, s: stream('a)) =>
  Detail.stateful(
    state =>
      switch (state) {
      | Some(acc) =>
        switch (next(s)) {
        | Some(x) =>
          let acc = List.tl(acc) @ [x];
          (Some(acc), Some(acc));
        | None => (None, None)
        }
      | None =>
        let acc = to_list(take(n, s));
        List.length(acc) < n ? (None, None) : (Some(acc), Some(acc));
      },
    None,
  );

let window2 = (s: stream('a)) =>
  s
  |> window(2)
  |> map(frame =>
       switch (frame) {
       | [a, b] => (a, b)
       | _ => assert(false)
       }
     );

let window3 = (s: stream('a)) =>
  s
  |> window(3)
  |> map(frame =>
       switch (frame) {
       | [a, b, c] => (a, b, c)
       | _ => assert(false)
       }
     );

let batch = (n: int, s: stream('a)) =>
  Detail.stateful(
    state =>
      switch (state) {
      | Some(s) =>
        let acc = to_list(take(n, s));
        (List.length(acc) < n ? None : Some(s), Some(acc));
      | None => (None, None)
      },
    Some(s),
  );

let split = (s: stream(('a, 'b))) => {
  let buf = to_list(s);
  (
    Stream.of_list(buf) |> map(((a, _)) => a),
    Stream.of_list(buf) |> map(((_, b)) => b),
  );
};

let fold_left_result =
    (fn: ('acc, 'a) => 'acc, init: 'acc, s: stream(result('a, 'b))) =>
  Util.loop_until(
    ((s, init)) =>
      switch (next(s)) {
      | Some(r) =>
        switch (r) {
        | Ok(x) => Ok((s, fn(init, x)))
        | Error(err) => Error(Error(err))
        }
      | None => Error(Ok(init))
      },
    (s, init),
  );
