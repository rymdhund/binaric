let ( >>= ) = CCResult.( >>= )
let ( >>| ) = CCResult.( >|= )

module Literals = struct
  let get_value v =
    match v with
    | `Numeric s -> Ok s
    | `String s ->
        Error (Format.sprintf "Invalid string \"%s\", expected numeric" s)


  let d8 v =
    get_value v
    >>= Util.int_of_string_res
    >>= fun n ->
    if n > 255 || n < -254
    then Error (Format.sprintf "d8: %d is out of range" n)
    else Ok (CCString.of_char @@ char_of_int (n land 0xff))


  let d16 v =
    get_value v
    >>= Util.int_of_string_res
    >>= fun n ->
    if n > 0xffff || n <= -0xffff
    then Error (Format.sprintf "d16: %d is out of range" n)
    else
      Ok
        (CCString.of_list
           [ char_of_int ((n lsr 8) land 0xff); char_of_int (n land 0xff) ])


  let d32 v =
    get_value v
    >>= Util.int32_of_string_res
    >>= fun n -> Ok (Util.chars_of_int32 n)


  let h8 v =
    get_value v
    >>= Util.int_of_hex_res
    >>= fun n ->
    if n > 0xff || n < 0
    then Error (Format.sprintf "h8: %x is out of range" n)
    else Ok (CCString.of_char @@ char_of_int n)


  let asc v =
    match v with
    | `Numeric s ->
        Error (Format.sprintf "Invalid numeric \"%s\", expected string" s)
    | `String s -> Ok s
end

module StringMap = CCMap.Make (CCString)

module Output = struct
  type statement =
    | Empty
    | Anonymous of expression
    | Label of (string * expression)

  and expression =
    | Plain of string
    | Block of statement list
    | Repeat of expression * int

  let override (orig : expression) (over : expression) : expression =
    let rec map_of_expr_out ns map (expr_out : expression) =
      match expr_out with
      | Plain _ -> map
      | Repeat (expr, _) -> map_of_expr_out ns map expr
      | Block stmt_outs ->
          List.fold_left
            (fun map1 stmt_out -> map_of_stmt_out ns map1 stmt_out)
            map
            stmt_outs
    and map_of_stmt_out ns map (stmt_out : statement) =
      match stmt_out with
      | Label (lbl, expr_out) ->
          let expr_map = map_of_expr_out (ns ^ lbl ^ ".") map expr_out in
          let merged =
            StringMap.union (fun _key _v1 v2 -> Some v2) map expr_map
          in
          StringMap.add (ns ^ lbl) expr_out merged
      | Empty | Anonymous _ -> map
    in
    let out_map = map_of_expr_out "" StringMap.empty over in
    let rec override_expr ns orig =
      match orig with
      | Plain _ as p -> p
      | Repeat (expr, n) -> Repeat (override_expr ns expr, n)
      | Block stmts -> Block (CCList.map (override_stmt ns) stmts)
    and override_stmt ns orig : statement =
      match orig with
      | Empty -> Empty
      | Anonymous _ as a -> a
      | Label (lbl, expr) ->
        ( match StringMap.find_opt (ns ^ lbl) out_map with
        | Some out -> Label (lbl, out)
        | None -> Label (lbl, override_expr (ns ^ lbl ^ ".") expr) )
    in
    override_expr "" orig


  let plain (s : string) : expression = Plain s

  let repeat (n : int) (output : expression) : expression = Repeat (output, n)

  let rec to_string (output : expression) : string =
    match output with
    | Plain out -> out
    | Block stmt_outs ->
        stmt_outs
        |> CCList.map (fun o -> stmt_to_string o)
        |> CCString.concat ""
    | Repeat (out, n) -> CCString.repeat (to_string out) n


  and stmt_to_string (output : statement) : string =
    match output with
    | Empty -> ""
    | Anonymous expr -> to_string expr
    | Label (_, expr) -> to_string expr
end

module Env = struct
  type t = Output.expression StringMap.t

  let add = StringMap.add

  let empty = StringMap.empty

  let get = StringMap.find_opt

  let add_wrapped (namespace : string) (wrap : t) (env : t) : t =
    StringMap.fold
      (fun key value env -> StringMap.add (namespace ^ "." ^ key) value env)
      wrap
      env
end

type expr_return = (Env.t * Output.expression, string) result

type stmt_return = (Env.t * Output.statement, string) result

let eval_load (name : string) (env : Env.t) : expr_return =
  match Env.get name env with
  | Some output -> Ok (Env.empty, output)
  | None -> Error (Format.sprintf "Unknown identifier '%s'" name)


(* TODO: split computation and load *)
let eval_computation (comp : Ast.computation) (env : Env.t) : expr_return =
  let evaluate f xs =
    Util.result_map f xs
    >>| CCString.concat ""
    >>| Output.plain
    >>| fun output -> (Env.empty, output)
  in
  match comp with
  | Ast.{ identifier; parameters = []; _ } -> eval_load identifier env
  | Ast.{ identifier; parameters; _ } ->
      let open Literals in
      ( match identifier with
      | "d8" -> evaluate d8 parameters
      | "h8" -> evaluate h8 parameters
      | "d16" -> evaluate d16 parameters
      | "d32" -> evaluate d32 parameters
      | "asc" -> evaluate asc parameters
      | id -> Error (Format.sprintf "Unknown identifier '%s'" id) )


let rec eval_statement (stmt : Ast.statement) (env : Env.t) : stmt_return =
  match stmt with
  | Anonymous expr ->
      eval_expression expr env
      >>| fun (_inner_env, output) -> (env, Output.Anonymous output)
  | Label (label, expr) ->
      eval_expression expr env
      >>| fun (_inner_env, output) -> (env, Output.Label (label, output))
  | Assignment (key, expr) ->
      eval_expression expr env
      >>| fun (inner_env, output) ->
      let new_env : Env.t =
        Env.add_wrapped key inner_env env |> Env.add key output
      in
      (new_env, Output.Empty)


and eval_expression (expr : Ast.expression) (env : Env.t) : expr_return =
  match expr with
  | Ast.Computation c -> eval_computation c env
  | Block stmts -> eval_block stmts env
  | Override (original, override) ->
      eval_expression override env
      >>= fun (_env, out_override) ->
      eval_expression original env
      >>| fun (inner_env, out_orig) ->
      (inner_env, Output.override out_orig out_override)
  | Repeat (expr, n) ->
      eval_expression expr env
      >>| fun (inner_env, out) -> (inner_env, Output.Repeat (out, n))
  | Import _file -> raise (Invalid_argument "Cant eval an import")
  | ImportBlock stmts -> eval_block stmts Env.empty


(* TODO: raise exception *)
and eval_block (stmts : Ast.statement list) (env : Env.t) : expr_return =
  CCList.fold_left
    (fun acc stmt ->
      acc
      >>= fun (env, sum_output) ->
      eval_statement stmt env
      >>| fun (new_env, output) ->
      (new_env, CCList.append sum_output [ output ]) )
    (Ok (env, []))
    stmts
  >>| fun (env, outputs) -> (env, Output.Block outputs)
