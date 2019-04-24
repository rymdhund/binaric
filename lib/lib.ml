let ( >>= ) = CCResult.( >>= )
let ( >>| ) = CCResult.( >|= )

module Args = Args

let eval_file ?(force = false) in_file out_file =
  try
    let dir = Filename.dirname in_file in
    CCIO.(
      with_in in_file (fun ic ->
          let _unconsumed, res = Angstrom_unix.parse Parse.program ic in
          res
          >>= fun ast ->
          Link.link_expr dir ast
          >>= fun ast_linked ->
          Eval.eval_expression ast_linked Eval.Env.empty
          >>= fun (_, output) ->
          let flags =
            if force
            then [ Open_binary; Open_creat ]
            else [ Open_binary; Open_creat; Open_excl ]
          in
          with_out ~flags ~mode:0o644 out_file (fun oc ->
              let consumer = output_string oc in
              Eval.Output.write_out consumer output ) ))
  with
  | Sys_error msg -> Error msg


let run ?(dir = ".") (prog : string) : (string, string) result =
  Angstrom.parse_string Parse.program prog
  >>= fun ast ->
  Link.link_expr dir ast
  >>= fun ast_linked ->
  Eval.eval_expression ast_linked Eval.Env.empty
  >>= fun (_, output) ->
  let buffer = Buffer.create 64 in
  let consumer = Buffer.add_string buffer in
  Eval.Output.write_out consumer output
  >>= fun _ -> Ok (Buffer.contents buffer)
