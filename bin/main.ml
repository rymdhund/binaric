let pair x y = x, y

let lift2 f at bt =
  match at, bt with
  | Some x, Some y -> Some (f x y)
  | _ -> None

let lift3 f at bt ct =
  let abt = lift2 pair at bt in
  let f2 = fun (a, b) c  -> f a b c in
  lift2 f2 abt ct

let foo a b c =
  a ^ b ^ c

let () =
  print_endline "hello";
  match lift3 foo (Some "a") (Some "b") (Some "c") with
  | Some x -> print_endline x
  | None -> print_endline "nothing"
