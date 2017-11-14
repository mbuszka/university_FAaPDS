module Deque: Signatures.DEQUE =
struct
  type 'a t = 'a list * 'a list

  let empty = [], []

  let isEmpty q =
    match q with
    | [], [] -> true
    | _      -> false

  let rec splitAt k ls =
    match k, ls with
    | 0, rest -> ([], rest)
    | _, [] -> ([], [])
    | k, (x::xs) ->
      let l, r = splitAt (k - 1) xs
      in x::l, r

  let bal q =
    match q with
    | [], r ->
      let k = List.length r / 2 in
      let b, f = splitAt k r
      in List.rev f, b
    | f, [] ->
      let k = (List.length f + 1) / 2 in
      let f', b = splitAt k f
      in f', List.rev b
    | q -> q

  exception EmptyQueue

  let cons e (f, b) = bal (e::f, b)

  let head q = match q with
    | x::_, _ -> x
    | _       -> raise EmptyQueue

  let tail q = match q with
    | _::f, b -> bal (f, b)
    | _       -> raise EmptyQueue

  let snoc (f, b) e = bal (f, e::b)

  let daeh q = match q with
    | _, x::_ -> x
    | [x], [] -> x
    | _       -> raise EmptyQueue

  let liat q = match q with
    | f, _::b -> bal (f, b)
    | [x], [] -> [], []
    | _ -> raise EmptyQueue
end

(*
  Amortized proof:
  Let p(f, b) = abs(length f - length b)
    - p is nonnegative and p([], []) = 0
  cost of
   - snoc, cons = 1 + dp = 2 (* we may prepend to the longer list,
                                thus increasing the difference *)

   - tail, liat = 1 + bal (* we may increase the potential by one *)

   - bal = 1  (* happens when one of the lists is empty, splitting non-empty
                 splitting the other one in half, thus changing potential to
                 0 or 1 *)
*)
