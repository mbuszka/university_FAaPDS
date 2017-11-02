use "heap.sml";

(* Exercise 18 *)
functor ExplicitMin (Heap: HEAP): HEAP =
struct
  structure Elem = Heap.Elem
  datatype Heap = Empty | NonEmpty of Elem.T * Heap.Heap
  exception EMPTY

  val empty = Empty
  fun isEmpty Empty = true
    | isEmpty _     = false

  fun insert (x, Empty) = NonEmpty (x, Heap.empty)
    | insert (x, NonEmpty (min, heap)) = if Elem.leq (x, min)
        then NonEmpty (x, Heap.insert (min, heap))
        else NonEmpty (min, Heap.insert (x, heap))
  fun merge (h, Empty) = h
    | merge (Empty, h) = h
    | merge (NonEmpty (m1, h1), NonEmpty (m2, h2)) =
      let
        val rest = Heap.merge (h1, h2)
      in
        if Elem.leq (m1, m2)
        then NonEmpty (m1, Heap.insert (m2, rest))
        else NonEmpty (m2, Heap.insert (m1, rest))
      end

  fun fromList [] = Empty
    | fromList (x :: xs) = NonEmpty (x, Heap.fromList xs)

  fun findMin Empty = raise EMPTY
    | findMin (NonEmpty (x, _)) = x

  fun deleteMin Empty = raise EMPTY
    | deleteMin (NonEmpty (x, h)) =
      let
        val m = Heap.findMin h
        val h2 = Heap.deleteMin h
      in
        NonEmpty (m, h2)
      end
end
