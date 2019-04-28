let ( >>= ) = CCResult.( >>= )
let ( >>| ) = CCResult.( >|= )

let int_min x y = if x < y then x else y

let rec result_map (f : 'a -> ('b, string) result) (xs : 'a list) :
    ('b list, string) result =
  match xs with
  | [] -> Ok []
  | x :: xs ->
      f x >>= fun head -> result_map f xs >>= fun tail -> Ok (head :: tail)


let range_seq (n : int) : 'a Seq.t =
  let rec range_seq' cur () =
    if cur = n then Seq.Nil else Seq.Cons (cur, range_seq' (cur + 1))
  in
  range_seq' 0
