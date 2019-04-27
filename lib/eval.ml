let ( >>= ) = CCResult.( >>= )
let ( >>| ) = CCResult.( >|= )

module Literals = struct
  module Decoders = struct
    let get_num = function
      | `Numeric s -> Ok s
      | `String s ->
          Error (Format.sprintf "Invalid string \"%s\", expected numeric" s)


    let get_str = function
      | `Numeric s ->
          Error (Format.sprintf "Invalid numeric \"%s\", expected string" s)
      | `String s -> Ok s


    let dec v : (int, string) result = get_num v >>= Util.int_of_string_res

    let dec32 v : (Int32.t, string) result =
      get_num v >>= Util.int32_of_string_res


    let dec64 v : (Int64.t, string) result =
      get_num v >>= Util.int64_of_string_res


    let hex v : (int, string) result = get_num v >>= Util.int_of_hex_res

    let hex32 v : (Int32.t, string) result =
      get_num v >>= Util.int32_of_hex_res


    let hex64 v : (Int64.t, string) result =
      get_num v >>= Util.int64_of_hex_res


    (* The input string is already assumed to be in utf-8 *)
    let utf8 v : (string, string) result = get_str v
  end

  module Encoders = struct
    let i8 n : (string, string) result =
      if n > 255 || n < -254
      then Error "out of range"
      else Ok (CCString.of_char @@ char_of_int (n land 0xff))


    let i16 n : (string, string) result =
      if n > 0xffff || n <= -0xffff
      then Error "out of range"
      else
        Ok
          (CCString.of_list
             [ char_of_int ((n lsr 8) land 0xff); char_of_int (n land 0xff) ])


    let i24 n : (string, string) result =
      if n > 0xffffff || n <= -0xffffff
      then Error "out of range"
      else
        Ok
          (CCString.of_list
             [ char_of_int ((n lsr 16) land 0xff);
               char_of_int ((n lsr 8) land 0xff);
               char_of_int (n land 0xff)
             ])


    let i32 n : (string, string) result = Ok (Util.chars_of_int32 n)

    let i64 n : (string, string) result = Ok (Util.chars_of_int64 n)

    let recode out_encoding (src : string) (dst : Buffer.t) =
      let rec loop d e =
        match Uutf.decode d with
        | `Uchar _ as u ->
            ignore (Uutf.encode e u) ;
            loop d e
        | `End -> ignore (Uutf.encode e `End)
        | `Malformed _ ->
            ignore (Uutf.encode e (`Uchar Uutf.u_rep)) ;
            loop d e
        | `Await -> assert false
      in
      let d = Uutf.decoder ~encoding:`UTF_8 (`String src) in
      let e = Uutf.encoder out_encoding (`Buffer dst) in
      loop d e


    let utf8 s : (string, string) result =
      let buffer = Buffer.create (String.length s) in
      recode `UTF_8 s buffer ;
      Ok (Buffer.contents buffer)


    let utf16_be s : (string, string) result =
      let buffer = Buffer.create (String.length s) in
      recode `UTF_16BE s buffer ;
      Ok (Buffer.contents buffer)


    let utf16_le s : (string, string) result =
      let buffer = Buffer.create (String.length s) in
      recode `UTF_16LE s buffer ;
      Ok (Buffer.contents buffer)
  end

  let get_raw v =
    match v with
    | `Numeric s -> s
    | `String s -> s


  let literal identifier values : (string, string) result =
    let dec_enc decoder encoder v =
      match decoder v >>= fun decoded -> encoder decoded with
      | Ok _ as k -> k
      | Error e ->
          Error (Printf.sprintf "%s: '%s' %s" identifier (get_raw v) e)
    in
    let apply decoder encoder xs =
      let de = dec_enc decoder encoder in
      Util.result_map de xs >>| CCString.concat ""
    in
    let parts = CCString.split ~by:"." identifier in
    let module E = Encoders in
    let module D = Decoders in
    match parts with
    | [ "i8"; "dec" ] -> apply D.dec E.i8 values
    | [ "i16"; "dec" ] -> apply D.dec E.i16 values
    | [ "i24"; "dec" ] -> apply D.dec E.i24 values
    | [ "i32"; "dec" ] -> apply D.dec32 E.i32 values
    | [ "i64"; "dec" ] -> apply D.dec64 E.i64 values
    | [ "i8"; "hex" ] -> apply D.hex E.i8 values
    | [ "i16"; "hex" ] -> apply D.hex E.i16 values
    | [ "i24"; "hex" ] -> apply D.hex E.i24 values
    | [ "i32"; "hex" ] -> apply D.hex32 E.i32 values
    | [ "i64"; "hex" ] -> apply D.hex64 E.i64 values
    | [ "utf8" ] -> apply D.utf8 E.utf8 values
    | [ "utf16" ] | [ "utf16"; "be" ] -> apply D.utf8 E.utf16_be values
    | [ "utf16"; "le" ] -> apply D.utf8 E.utf16_le values
    | _ -> Error (Format.sprintf "Unknown identifier '%s'" identifier)
end

module StringMap = CCMap.Make (CCString)

module Output = struct
  type statement =
    | Empty
    | Anonymous of expression
    | Label of (string * expression)

  and expression =
    | Plain of string
    | ImportRaw of string * int option * int option
    | Block of statement list
    | Repeat of expression * int

  let override (orig : expression) (over : expression) : expression =
    let rec map_of_expr_out ns map (expr_out : expression) =
      match expr_out with
      | Plain _ -> map
      | ImportRaw _ -> map
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
      | ImportRaw _ as r -> r
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

  let rec write_out (consumer : string -> unit) (output : expression) :
      (unit, string) result =
    let read_chunks_max ~max ic =
      let size = 1024 in
      let buf = Bytes.create size in
      let count = ref max in
      let next () =
        if !count = 0
        then None
        else
          let n = input ic buf 0 (Util.int_min size !count) in
          count := !count - n ;
          if n = 0 then None else Some (Bytes.sub_string buf 0 n)
      in
      next
    in
    match output with
    | Plain out ->
        consumer out ;
        Ok ()
    | ImportRaw (file, start, end_) ->
      ( try
          CCIO.with_in file (fun ic ->
              ( match start with
              | None -> ()
              | Some n -> seek_in ic n ) ;
              let chunks =
                match (end_, start) with
                | None, _ -> CCIO.read_chunks ic
                | Some n, Some m -> read_chunks_max ~max:(n - m) ic
                | Some n, None -> read_chunks_max ~max:n ic
              in
              Gen.iter consumer chunks ;
              Ok () )
        with
      | Sys_error msg -> Error msg )
    | Block stmt_outs ->
        CCList.fold_left
          (fun res o -> res >>= fun () -> stmt_write_out consumer o)
          (Ok ())
          stmt_outs
    | Repeat (out, n) ->
        Seq.fold_left
          (fun res _ -> res >>= fun () -> write_out consumer out)
          (Ok ())
          (Util.range_seq n)


  and stmt_write_out consumer (output : statement) : (unit, string) result =
    match output with
    | Empty -> Ok ()
    | Anonymous expr -> write_out consumer expr
    | Label (_, expr) -> write_out consumer expr
end

(** The environment holds the assigned variables *)
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
  match comp with
  | Ast.{ identifier; parameters = []; _ } -> eval_load identifier env
  | Ast.{ identifier; parameters; _ } ->
      Literals.literal identifier parameters
      >>| fun output -> (Env.empty, Output.plain output)


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
  | ImportRaw (file, start, end_) ->
      Ok (Env.empty, Output.ImportRaw (file, start, end_))
  | ImportBlock stmts -> eval_block stmts Env.empty


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
