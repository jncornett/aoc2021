#!/bin/sh

for i in $(seq 1 25); do
  pad="$(printf "%02d" "$i")"
  mkdir -pv "Bin$pad"
  cat >"Bin$pad/dune" <<EOF
(executable
  (name Bin$pad)
  (public_name Bin$pad)
  (libraries console.lib lib))
EOF
  if ! test -f "Bin$pad/Bin$pad.re"; then
    cat >"Bin$pad/Bin$pad.re" <<EOF
Console.log("Running Test Program #$i:");
let () = print_endline(Lib.Util.hello());
EOF
  fi
done
