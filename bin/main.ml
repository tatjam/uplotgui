(* Copyright (C) 2026 tatjam
   SPDX-License-Identifier: GPL-3.0-or-later *)

let () =
   let char_flags = ['a'] in
   let char_arg_flags = ['d'] in
   let long_flags = ["--guamedo"] in
   let long_arg_flags = ["--guamedo-was-here"] in
   print_endline (ProgramArgs.show_arguments (ProgramArgs.parse char_flags char_arg_flags long_flags long_arg_flags))
