let main (outfile : string option) (force : bool) (infile : string) : unit =
  let drop_ending filename =
    let len = CCString.length filename in
    if len > 3 && CCString.suffix ~suf:".bn" filename
    then Some (CCString.sub filename 0 (len - 3))
    else None
  in
  let outfile =
    match (outfile, drop_ending infile) with
    | Some f, _ -> f
    | None, Some f -> Filename.basename f
    | None, None -> "out.bin"
  in
  match Lib.eval_file infile outfile ~force with
  | Ok () -> ()
  | Error msg ->
      Printf.eprintf "Error: %s\n" msg ;
      exit 1


let () =
  let open Lib.Args in
  let prog =
    program main
    <*> flag_param "o" "file" ~doc:"Output file"
    <*> flag "f" ~alias:[ "force" ] ~doc:"Force overwrite of output file"
    <*> positional "source" ~doc:"Source .bn file to be compiled"
  in
  run prog
