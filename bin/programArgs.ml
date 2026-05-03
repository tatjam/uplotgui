type range = float * float [@@deriving show]

module Lines = struct
  type t = {
    xlim : range option;
    ylim : range option;
    fmt : string option;
    grid : bool option;
  }
  [@@deriving show]

  let default = { xlim = None; ylim = None; fmt = None; grid = None }

  let parse_mode_arg arg last_arg (lines : t) : t = lines
end

module ModeArgs = struct
  type t = Lines of Lines.t | Unparsed [@@deriving show]

  let default = Unparsed
end

module BaseArgs = struct
  type t = {
    pass : string option;
    output : string option;
    delim : string option;
    headers : bool option;
    transpose : bool option;
    title : string option;
    xlabel : string option;
    ylabel : string option;
    config : string option;
  }
  [@@deriving show]

  let default =
    {
      pass = None;
      output = None;
      delim = None;
      headers = None;
      transpose = None;
      title = None;
      xlabel = None;
      ylabel = None;
      config = None;
    }

  (* Parse a general arg, returning the updated t or it unchanged *)
  let parse_general_arg arg last_arg (base_args : t) : t =
    match last_arg with
    | Some "-O" | Some "--pass" -> { base_args with pass = arg}
    | Some _ -> base_args
    | None -> ( match arg with _ -> base_args)
end

type t = { base : BaseArgs.t; mode : ModeArgs.t } [@@deriving show]

let default = { base = BaseArgs.default; mode = ModeArgs.default }

(* Parse a mode arg, the logic returns "Unparsed" if the argument is not to be parsed here *)
let parse_mode_arg arg last_arg (current_mode : ModeArgs.t) : ModeArgs.t =
  match current_mode with
  | Lines l -> ModeArgs.Lines (Lines.parse_mode_arg arg last_arg l)
  | Unparsed -> (
      match arg with "lines" -> ModeArgs.Lines Lines.default | _ -> Unparsed)


(* Parse an arg, returning the updated t or not changing it *)
let parse_arg arg last_arg (current_result : t) : t =
  let try_mode = parse_mode_arg arg last_arg current_result.mode in
  match try_mode with
  | Unparsed -> parse_general_arg arg last_arg current_result
  | _ -> { current_result with mode = try_mode }

(* Parse the arguments passed, including the last parsed argument, returning the updated t or not changing it *)
let rec parse_args args last_arg (current_result : t) : t =
  match args with
  | [] -> current_result
  | arg :: rest ->
      let updated = parse_arg arg last_arg current_result in
      parse_args rest (Some arg) updated

let parse () : t =
  let args = Sys.argv |> Array.to_list |> List.tl in
  parse_args args None default
