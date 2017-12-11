use "ordered.sml";
use "stream.sml";

open Stream;

functor Ex54 (Element: ORDERED) =
struct
  structure Elem = Element

  datatype Tree = Node of Elem.T * Tree list
  datatype Digit = Zero | One of Tree
  type Schedule = Digit Stream list
  type Heap = Digit Stream * Schedule

  fun link (t1 as Node (x, xs), t2 as Node (y, ys)) =
        if Elem.leq(x, y) then Node (x, t2 :: xs) else Node (y, t1 :: ys)
  
  fun insTree (t, $Nil) = $(Cons(One t, $Nil))
    | insTree (t, $(Cons(One t2, ds))) = $(Cons(Zero, insTree(link (t, t2), ds)))
    | insTree (t, $(Cons(Zero, ds))) = $(Cons(One t, ds))

  fun mrgList (x::xs, $(Cons(Zero, ds))) = $(Cons(One x, mrgList (xs, ds)))
    | mrgList (x::xs, $(Cons(One t, ds))) = let
        val ds' = insTree (link (x, t), ds)
        in $(Cons(Zero, mrgList (xs, ds'))) end
    | mrgList (x::xs, $Nil) = $(Cons(One x, mrgList (xs, $Nil)))
    | mrgList ([], ds) = ds

end