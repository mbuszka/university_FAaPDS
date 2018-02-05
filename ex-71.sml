use heap.sml;

functor SegBinHeap (Element: ORDERED): HEAP =
struct
  structure Elem = Element;

  datatype Tree = Node of Elem.T * Tree list
  datatype Digit
    = Zero
    | Ones of int * Tree list
    | Two  of Tree * Tree

  type Heap = Digit list

  val empty = []

  fun isEmpty [] = true
    | isEmpty _  = false

  fun link (t1 as Node (r1, ts1), t2 as Node (r2, ts2)) =
      if Elem.leq (r1, r2)
      then Node (r1, t2 :: ts1)
      else Node (r2, t1 :: ts2)


end