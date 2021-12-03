let lines = (chan: in_channel) =>
  Stream.from(_ =>
    try(Some(input_line(chan))) {
    | End_of_file => None
    }
  );
