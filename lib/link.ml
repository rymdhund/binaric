let ( >>= ) = CCResult.( >>= )
let ( >>| ) = CCResult.( >|= )

let parse_file filename =
  try
    CCIO.(
      with_in filename (fun ic ->
          let _unconsumed, res = Angstrom_unix.parse Parse.program ic in
          res ))
  with
  | Sys_error msg -> Error msg


(* Here we substitute all Ast.Imports with Ast.ImportBlocks *)
let rec link_expr dir (expr : Ast.expression) =
  match expr with
  | Ast.Import filename ->
      Printf.printf "cwd: %s\n" (Sys.getcwd ()) ;
      let file = Filename.concat dir filename in
      let newdir = Filename.dirname file in
      parse_file file
      >>= (function
      | Ast.Block stmts -> link_expr newdir (Ast.ImportBlock stmts)
      | _ ->
          raise (Invalid_argument "Parse program is supposed to give a block")
      )
  | Ast.Block stmts ->
      Util.result_map (link_stmt dir) stmts >>| fun stmts1 -> Ast.Block stmts1
  | Ast.ImportBlock stmts ->
      Util.result_map (link_stmt dir) stmts
      >>| fun stmts1 -> Ast.ImportBlock stmts1
  | Ast.Override (expr, over) ->
      link_expr dir expr >>| fun expr1 -> Ast.Override (expr1, over)
  | Ast.Repeat (expr, n) ->
      link_expr dir expr >>| fun expr1 -> Ast.Repeat (expr1, n)
  | e -> Ok e


and link_stmt dir (stmt : Ast.statement) =
  match stmt with
  | Ast.Anonymous expr ->
      link_expr dir expr >>| fun expr1 -> Ast.Anonymous expr1
  | Ast.Label (lbl, expr) ->
      link_expr dir expr >>| fun expr1 -> Ast.Label (lbl, expr1)
  | Ast.Assignment (var, expr) ->
      link_expr dir expr >>| fun expr1 -> Ast.Assignment (var, expr1)
