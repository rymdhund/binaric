module Spec = struct
  type doc = string option [@@deriving eq]

  type item =
    | Flag of (string list * doc)
    | FlagParam of (string list * string * doc)
    | Positional of (string * doc)
  [@@deriving eq]

  type t = {
    name: string option;
    items: item list;
  }

  let empty = {
    name = None;
    items = [];
  }

  let of_item item = {
    name = None;
    items = [item];
  }

  let dashed option_name =
    match CCString.length option_name with
    | 1 -> Format.sprintf "-%s" option_name
    | _ -> Format.sprintf "--%s" option_name

  let show_item (item:item) =

    let show_option_flag names _doc =
      Format.sprintf "[%s]" (names |> CCList.map dashed |> CCString.concat " | ")
    in

    let show_option_value names parameter _doc =
      let decorate option_name =
          Format.sprintf "%s <%s>" (dashed option_name) parameter
      in

      Format.sprintf "[%s]" (names |> CCList.map decorate |> CCString.concat " | ")
    in

    let show_positional parameter _doc =
      Format.sprintf "<%s>" parameter
    in

    match item with
    | Flag (names, doc) -> show_option_flag names doc
    | FlagParam (names, parameter, doc) -> show_option_value names parameter doc
    | Positional (parameter, doc) -> show_positional parameter doc

  let usage (default_name:string) (spec:t) =
    let name = match spec.name with
    | Some n -> n
    | None -> default_name
    in
    Printf.sprintf "%s %s" name (
      spec.items
      |> CCList.map show_item
      |> CCString.concat " ")

  let find_flag (spec:t) arg =
    let option_eq opt_name =
      dashed opt_name = arg
    in
    let check = function
      | Flag (names, _) -> CCList.exists option_eq names
      | _ -> false
    in
    CCList.find_opt check spec.items

  let find_flag_param (spec:t) arg =
    let option_eq opt_name =
      dashed opt_name = arg
    in
    let check = function
      | FlagParam (names, _, _) -> CCList.exists option_eq names
      | _ -> false
    in
    CCList.find_opt check spec.items

  let combine (t1:t) (t2:t): t =
    {
      name = CCOpt.or_ t1.name ~else_:t2.name;
      items = CCList.append t1.items t2.items
    }
end

module Parse = struct
  type found_item = Spec.item * string
  type ast = found_item list (* "AST" *)

  let get_positionals (spec:Spec.t): Spec.item list =
    let is_pos = function
    | Spec.Positional _ -> true
    | _ -> false
    in
    CCList.filter is_pos spec.items

  let (>>=) = CCResult.(>>=)

  let rec parse_args (spec:Spec.t) (args:string list): (found_item list, _) result =

      let is_dashed arg =
        CCString.get arg 0 = '-'
      in

      match args with
      | arg :: args' when is_dashed arg -> (
        match Spec.find_flag spec arg, Spec.find_flag_param spec arg, args' with
        | None, None, _ -> Error (Printf.sprintf "No such option %s" arg)
        | Some flag, _, _ ->
          parse_args spec args' >>= fun found ->
          Ok ((flag, ""):: found)
        | None, Some _, [] ->
          Error (Printf.sprintf "Expected parameter for option %s" arg)
        | None, Some flag_param, (param::args') ->
          parse_args spec args' >>= fun found ->
          Ok ((flag_param, param):: found)
      )
      | args' -> parse_positional (get_positionals spec) args'
    and parse_positional spec args =
      let get_name item =
        match item with
        | Spec.Positional (name, _) -> name
        | _ -> raise (Failure "shold not happen")
      in
      match spec, args with
      | [], [] -> Ok []
      | exp :: _, [] -> Error (Printf.sprintf "Expected \"%s\" argument" (get_name exp))
      | [], arg :: _ -> Error (Printf.sprintf "Unexpected positional argument \"%s\"" arg)
      | exp :: spec', arg :: args' ->
        parse_positional spec' args' >>= fun found ->
        Ok ((exp, arg) :: found)
end

module Eval = struct
  let (>>=) = CCResult.(>>=)

  type 'a t = Parse.ast -> ('a, string) result

  let apply (f_t: ('a -> 'b) t) (a_t: 'a t) =
    fun args ->
      f_t args >>= fun f ->
      a_t args >>= fun a ->
      Ok (f a)

  let pure a =
    fun _args ->
      Ok a

  let flag item: bool t =
    fun args ->
      Ok (CCList.exists (fun (it, _val) -> Spec.equal_item item it) args)

  let flag_param item: string option t =
    fun args ->
      match CCList.find_opt (fun (it, _) -> Spec.equal_item item it) args with
      | Some (_item, value) -> Ok (Some value)
      | None -> Ok None

  let positional item: string t =
    fun args ->
      match CCList.find_opt (fun (it, _) -> Spec.equal_item item it) args with
      | Some (_item, value) -> Ok value
      (* If we managed to build an ast all arguments should be there *)
      | None -> raise (Failure "Unexpectedly did not find positional item")

  let run (eval: 'a t) (args:Parse.ast): ('a, string) result =
    eval args
end

let (>>=) = CCResult.(>>=)

type 'a t = {
  spec: Spec.t;
  eval: 'a Eval.t;
  name: string option;
}

let apply (ft: ('a -> 'b) t) (at: 'a t): 'b t =
  let spec = Spec.combine ft.spec at.spec in
  let eval = Eval.apply ft.eval at.eval in
  let name = CCOpt.or_ ft.name ~else_:at.name in
  {
    spec;
    eval;
    name;
  }

let (<*>) = apply

let pure a: 'a t = {
  spec = Spec.empty;
  eval = Eval.pure a;
  name = None;
}

let program ?name (main:'a): 'a t =
  let name = match name with
  | None -> Some Sys.argv.(0)
  | x -> x
  in
  {
  spec = Spec.empty;
  eval = Eval.pure main;
  name;
}

let usage (prog:'a t): string =
  let cmd = match prog.name with
  | Some n -> n
  | None -> ""
  in
  (Spec.usage cmd prog.spec)


let run (prog:unit t): unit =
  let args = CCArray.to_list Sys.argv |> CCList.drop 1 in
  match Parse.parse_args prog.spec args >>= fun preproc -> Eval.run prog.eval preproc with
  | Ok () -> ()
  | Error msg ->
      Printf.eprintf "%s\n" msg;
      Printf.eprintf "Usage: %s\n" (usage prog);
      exit 1

let flag ?doc ?(alias=[]) name: bool t =
  let item = Spec.Flag (name :: alias, doc) in
  {
    spec = Spec.of_item item;
    eval = Eval.flag item;
    name = None;
  }

let flag_param ?doc ?(alias=[]) name param: string option t =
  let item = Spec.FlagParam (name :: alias, param, doc) in
  {
    spec = Spec.of_item item;
    eval = Eval.flag_param item;
    name = None;
  }

let positional ?doc param: string t =
  let item = Spec.Positional (param, doc) in
  {
    spec = Spec.of_item item;
    eval = Eval.positional item;
    name = None;
  }
