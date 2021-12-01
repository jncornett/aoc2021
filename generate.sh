#!/bin/sh

for i in $(seq 1 25); do
  mkdir -pv "Bin$i"
  cat >"Bin$i/dune" <<EOF
(executable
  (name Bin$i)
  (public_name Bin$i)
  (libraries console.lib lib))
EOF
  cat >"Bin$i/Bin$i.re" <<EOF
Console.log("Running Test Program #$i:");
let () = print_endline(Lib.Util.hello());
EOF
done
