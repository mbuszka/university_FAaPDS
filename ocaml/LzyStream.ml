type 'a stream_cell = Nil | Cons of 'a * 'a stream
and 'a stream = 'a stream_cell Lazy.t

let rec (++) a b =
  lazy (
      match a with
      | lazy Nil -> Lazy.force b
      | lazy (Cons (hd, tl)) -> Cons (hd, tl ++ b)
    )

let rec take n a =
  lazy (
      match n, a with
      | 0, _ -> Nil
      | _, lazy Nil -> Nil
      | _, lazy (Cons (hd, tl)) -> Cons (hd, take (n-1) tl)
    )

let rec drop n a =
  lazy (
      match n, a with
      | 0, _ -> Lazy.force a
      | _, lazy Nil -> Nil
      | _, lazy (Cons (hd, tl)) -> Lazy.force (drop (n-1) tl)
    )

let reverse a =
  lazy (
      let rec rev a acc =
        match a with
        | lazy Nil -> acc
        | lazy (Cons (hd, tl)) -> rev tl (Cons (hd, lazy acc))
      in
      rev a Nil
    )

let const x =
  let rec s = lazy (Cons (x, s))
  in s

let rec map f str = lazy (
  match str with
  | lazy Nil -> Nil
  | lazy (Cons (x, xs)) -> Cons (f x, map f xs)
)
