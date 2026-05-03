(* Copyright (C) 2026 tatjam
   SPDX-License-Identifier: GPL-3.0-or-later *)

(* Simple parser-combinator types and helpers *)

type 'a parser = string list -> ('a * string list) option

let checked_token (f : string -> bool) : string parser = function
  | [] -> None
  | x :: xs when f x = true -> Some (x, xs)
  | _ :: _ -> None

let matching (name : string) : string parser = checked_token (fun x -> x = name)

let optional (a : 'a parser) : 'a option parser =
 fun x ->
  match a x with
  | Some (ax, xs) -> Some (Some ax, xs)
  | None -> Some (None, x)

let either (a : 'a parser) (b : 'a parser) : 'a parser =
 fun x ->
  match a x with
  | Some ax -> Some ax
  | None -> b x

let any_of (ps : 'a parser list) : 'a parser =
 fun x -> List.find_map (fun p -> p x) ps

let both (a : 'a parser) (b : 'b parser) : ('a * 'b) parser =
 fun x ->
  match a x with
  | Some (ax, xs) -> (
      match b xs with
      | Some (bx, xss) -> Some ((ax, bx), xss)
      | None -> None)
  | None -> None

let rec zero_or_more (a : 'a parser) : 'a list parser =
 fun x ->
  match a x with
  | Some (ax, xs) ->
      Option.map (fun (axs, xss) -> (ax :: axs, xss)) (zero_or_more a xs)
  | None -> Some ([], x)

let chain (f : 'a -> 'b option) (a : 'a parser) : 'b parser =
 fun x ->
  match a x with
  | Some (ax, xs) -> (
      match f ax with
      | Some fax -> Some (fax, xs)
      | None -> None)
  | None -> None

let map (f : 'a -> 'b) (a : 'a parser) : 'b parser =
 fun x ->
  match a x with
  | Some (ax, xs) -> Some (f ax, xs)
  | None -> None

(* Argument types *)

type flag = SimpleFlag of string| OptionFlag of string * string
[@@deriving show]

(* Returns true if flag is set (present), false otherwise *)
let get_simple_flag (short : string) (long : string) (flags : flag list) : bool
    =
  Option.is_some
    (List.find_opt
       (fun flag ->
         match flag with
         | SimpleFlag f -> f = short || f = long
         | _ -> false)
       flags)

(* Returns the option value, or None *)
let get_option_value (short : string) (long : string) (flags : flag list) :
    string option =
  Option.map
    (fun f ->
      match f with
      | OptionFlag (_, v) -> v
      | _ -> assert false)
    (List.find_opt
       (fun flag ->
         match flag with
         | OptionFlag (n, v) -> n = short || n = long
         | _ -> false)
       flags)

type arguments = { flags : flag list; command : string; file : string option }
[@@deriving show]

(* Token types *)

(* A token of the type -x *)
let short_flag_token (flags : char list) : string parser =
  checked_token (fun s ->
      if String.starts_with ~prefix:"-" s && String.length s = 2 then
        let flag = s.[1] in
        List.mem flag flags
      else false)

(* A token of the type --flag *)
let long_flag_token (flags : string list) : string parser =
  checked_token (fun x ->
      if String.starts_with ~prefix:"--" x then
        let flag = String.sub x 2 (String.length x - 2) in
        List.mem flag flags
      else false)

(* Anything that's not -x or --flag *)
let not_flag_token : string parser =
  checked_token (fun s -> not (String.starts_with ~prefix:"-" s))

(* Allows --flag [value], capturing the (flag, value) pair *)
let long_value_flag_token (flags : string list) : flag parser =
  map (fun x -> OptionFlag x) (both (long_flag_token flags) not_flag_token)

(* Allows --flag *)
let long_simple_flag_token (flags : string list) : flag parser =
  map (fun x -> SimpleFlag x) (long_flag_token flags)

(* Allows -f [value], capturing the (flag, value) pair. Note, we preparse to split chains of single chars *)
let short_value_flag_token (flags : char list) : flag parser =
  map (fun x -> OptionFlag x) (both (short_flag_token flags) not_flag_token)

(* Allows -f. Note, we preparse to split chain of single chars*)
let short_simple_flag_token (flags : char list) : flag parser =
  map (fun x -> SimpleFlag x) (short_flag_token flags)

(* Allows -f, -g [value], --flag, --glag [value] *)
let any_flag_token (char_flags : char list) (char_arg_flags : char list)
    (long_flags : string list) (long_arg_flags : string list) : flag parser =
  any_of
    [
      short_simple_flag_token char_flags;
      short_value_flag_token char_arg_flags;
      long_simple_flag_token long_flags;
      long_value_flag_token long_arg_flags;
    ]

(* [flags?] <command> [flags?] <file?> *)
let parser (char_flags : char list) (char_arg_flags : char list)
    (long_flags : string list) (long_arg_flags : string list) : arguments parser
    =
  let flag_parser =
    any_flag_token char_flags char_arg_flags long_flags long_arg_flags
  in
  (* we first parse the [flags?] <command> [flags?] thing *)
  let first_parser =
    both
      (both (zero_or_more flag_parser) not_flag_token)
      (zero_or_more flag_parser)
  in
  (* flatten it *)
  let flattener =
   fun ((flags_before_command, command), flags_after_command) ->
    let all_flags = flags_before_command @ flags_after_command in
    (all_flags, command)
  in
  let flattened = map flattener first_parser in
  (* The entire parser including the optional file *)
  let parser = both flattened (optional not_flag_token) in
  (* Finally, we make it read the neat arguments record *)
  let recorder = fun ((flags, command), file) -> { flags; command; file } in
  map recorder parser

(* Preprocessor only, a flag of type -abdcdewathever, everything else is unprocessed *)
let is_preprocessor_flag_token (s : string) : bool =
  String.starts_with ~prefix:"-" s && not (String.starts_with ~prefix:"--" s)

(* Preprocess a string, splitting -abcdwathever -> -a -b -c -d wathever *)
let preprocess (char_arg_flags : char list) (args : string list) : string list =
  let replace_flag_token = function
    | s when is_preprocessor_flag_token s ->
        let rec parse_chars list = function
          | [] -> list
          | s :: xs ->
              let s_str = String.make 1 s in
              if List.mem s char_arg_flags then
                if List.length xs > 0 then
                  let xs_str = xs |> List.to_seq |> String.of_seq in
                  xs_str :: ("-" ^ s_str) :: list
                else ("-" ^ s_str) :: list
              else parse_chars (("-" ^ s_str) :: list) xs
        in
        let chars =
          String.sub s 1 (String.length s - 1) |> String.to_seq |> List.of_seq
        in
        List.rev (parse_chars [] chars)
    | s -> [ s ]
  in
  List.flatten (List.map replace_flag_token args)

(* Particular implementation *)
let parse (char_flags : char list) (char_arg_flags : char list)
    (long_flags : string list) (long_arg_flags : string list) =
  let parser = parser char_flags char_arg_flags long_flags long_arg_flags in
  let raw_args = List.tl (Sys.argv |> Array.to_list) in
  let args = preprocess char_arg_flags raw_args in
  let parsed = parser args in
  match parsed with
  | Some (args, []) -> args
  | Some (args, leftover) ->
      let leftover_str = String.concat " " leftover in
      Printf.eprintf "Unexpected arguments: %s\n" leftover_str;
      exit 1
  | None ->
      Printf.eprintf "Unable to parse arguments";
      exit 1
