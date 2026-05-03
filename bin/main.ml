(* Copyright (C) 2026 tatjam
   SPDX-License-Identifier: GPL-3.0-or-later *)

let () =
  let char_flags = [ 'H'; 'T'; 'p'; 'C'; 'M' (* count *); 'r'] in
  let char_arg_flags =
    [ 'O'; 'o'; 'd'; 't'; 'w'; 'h'; 'b'; 'm'; 'c'; (* histogram *) 'n' ]
  in
  let long_flags =
    [
      "headers";
      "transpose";
      "labels";
      "no-labels";
      "progress";
      "color-output";
      "monochromo";
      "help";
      "debug";
      (* line *)
      "--grid";
      "--no-grid";
      (* count *)
      "--reverse"
    ]
  in
  let long_arg_flags =
    [
      "pass";
      "output";
      "delimiter";
      "title";
      "xlabel";
      "ylabel";
      "width";
      "height";
      "border";
      "margin";
      "padding";
      "color";
      "encoding";
      "config";
      "fmt";
      (* line, scatter, density, boxplot *)
      "xlim";
      (* line, scatter, density *)
      "ylim";
      "canvas";
      (* barplot, count *)
      "xscale";
      "symbol";
      (* histogram *)
      "nbins";
      "closed";
      "symbol";
      
    ]
  in
  print_endline
    (ProgramArgs.show_arguments
       (ProgramArgs.parse char_flags char_arg_flags long_flags long_arg_flags))
