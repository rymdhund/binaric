let (>>=) = CCResult.(>>=)
let (>>|) = CCResult.(>|=)

module Args = Args

let eval_file ?(force=false) in_file out_file =
  try CCIO.(
    with_in in_file
      (fun ic ->
        let (_unconsumed, res) = Angstrom_unix.parse Parse.program ic in
        res >>= fun ast ->
        Eval.eval_expression ast Eval.Env.empty >>= fun (_, output) ->
        let flags = if force then
          [Open_binary; Open_creat ]
        else
          [Open_binary; Open_creat; Open_excl]
        in
          with_out ~flags ~mode:0o644 out_file
            (fun oc ->
              Printf.fprintf oc "%s" (Eval.Output.to_string output);
              Ok ()
            )
      )
  ) with
  | Sys_error msg -> Error msg

let run (prog:string): (string, string) result =
  Angstrom.parse_string Parse.program prog >>= fun ast ->
  Eval.eval_expression ast Eval.Env.empty >>| fun (_, output) -> Eval.Output.to_string output
