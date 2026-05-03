(* Copyright (C) 2026 tatjam
   SPDX-License-Identifier: GPL-3.0-or-later *)

type header = string

(* TODO: parse_header *)
let parse_header () : header * string =
  let first_line = input_line stdin in
  (first_line, first_line)

type plot_spec = string -> string

let write_sanitized_line (oc : out_channel) (header : header) (line : string) =
  output_string oc line;
  output_char oc '\n'

let gnuplot (args : ProgramArgs.arguments) (header : header)
    (cmds : plot_spec list) (first_line : string) =
  (* First dump to temp file *)
  let tmp = Filename.temp_file "uplot" ".dat" in
  let tmp_oc = open_out tmp in
  write_sanitized_line tmp_oc header first_line;
  (try
     while true do
       let line = input_line stdin in
       write_sanitized_line tmp_oc header line
     done
   with
  | End_of_file -> ());
  close_out tmp_oc;

  let oc = Unix.open_process_out "gnuplot -p" in
  output_string oc "set term qt persist\n";

  (* output_string oc "set term x11 persist\n"; *)
  (* output_string oc "set term wxt persist\n"; *)
  let delim =
    Option.value
      (ProgramArgs.get_option_value "-d" "--delimiter" args.flags)
      ~default:"\\t"
  in
  output_string oc ("set datafile separator \"" ^ delim ^ "\"\n");
  output_string oc "set datafile missing \"NaN\"\n";
  if ProgramArgs.get_simple_flag "-H" "--headers" args.flags then
    output_string oc "set key autotitle columnhead\n";

  List.iter (fun s -> output_string oc (s tmp)) cmds;

  output_string oc "pause mouse close\n";
  flush oc;

  ignore (Unix.close_process_out oc)

let line (args : ProgramArgs.arguments) =
  let header, first_line = parse_header () in
  gnuplot args header
    [ Printf.sprintf "plot '%s' with linespoints\n" ]
    first_line

(* TODO: Detect based on header *)
let lines (args: ProgramArgs.arguments) =
  let header, first_line = parse_header () in
  gnuplot args header
    [ fun s -> Printf.sprintf "plot '%s' using 1:2 with linespoints,\\\n'%s' using 1:3 with linespoints\n" s s ]
    first_line

let () =
  let char_flags = [ 'H'; 'T'; 'p'; 'C'; 'M' (* count *); 'r' ] in
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
      "grid";
      "no-grid";
      (* count *)
      "reverse";
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
  let args =
    ProgramArgs.parse char_flags char_arg_flags long_flags long_arg_flags
  in
  match args.command with
  | "barplot"
  | "bar" ->
      exit 1
  | "histogram"
  | "hist" ->
      exit 1
  | "lineplot"
  | "line" ->
      line args
  | "lineplots"
  | "lines" ->
      lines args
  | "scatter"
  | "s" ->
      exit 1
  | "density"
  | "d" ->
      exit 1
  | "boxplot"
  | "b" ->
      exit 1
  | "count"
  | "c" ->
      exit 1
  | s ->
      Printf.eprintf "Unknown command %s\n" s;
      exit 1
