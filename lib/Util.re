let hello = () =>
  Pastel.(
    <Pastel>
      <Pastel color=Red> "Hello" </Pastel>
      ", "
      <Pastel color=Green> "World" </Pastel>
      "!"
    </Pastel>
  );

let goodbye = () =>
  Pastel.(
    <Pastel>
      <Pastel color=Blue> "Goodbye" </Pastel>
      ", "
      <Pastel color=Green> "World" </Pastel>
      "!"
    </Pastel>
  );

let loop_until = (fn: 'state => result('state, 'rv), init: 'state) => {
  let state = ref(Ok(init));
  while (Result.is_ok(state^)) {
    state := fn(Result.get_ok(state^));
  };
  Result.get_error(state^);
};
