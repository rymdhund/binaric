let (>>=) = CCResult.(>>=)
let (>>|) = CCResult.(>|=)

let run (prog:string): (string, string) result =
  Angstrom.parse_string Parse.program prog >>= fun ast ->
  Eval.eval_expression ast Eval.Env.empty >>| fun (_, output) -> Eval.Output.to_string output
