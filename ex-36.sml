use "heap.sml";

functor SplayTree (Element: ORDERED) =
struct
  structure Elem = Element

  datatype Heap = E | T of Heap * Elem.T * Heap

  val empty = E

  fun isEmpty E = true
    | isEmpty _ = false

  fun bigger (pivot, E) = E
    | bigger (pivot, T (a, x, b)) =
        if Elem.leq (x, pivot)
        then bigger (pivot, b)
        else case a of
            E => T (E, x, b)
          | T (a1, y, a2) =>
            if Elem.leq (y, pivot)
            then T (bigger (pivot, a2), x, b)
            else T (bigger (pivot, a1), y, T (a2, x, b))

  fun smaller (pivot, E) = E
    | smaller (pivot, T (a, x, b)) =
        if Elem.leq (x, pivot)
        then case b of
            E => T (a, x, E)
          | T (b1, y, b2) =>
            if Elem.leq (y, pivot)
            then T (T (a, x, b1), y, smaller (pivot, b2))
            else T (a, x, smaller (pivot, b1))
        else smaller (pivot, a)

  fun insert (x, t) = T (smaller (x, t), x, bigger (x, t))

  exception EMPTY

  fun findMin E = raise EMPTY
    | findMin (T (E, x, _)) = x
    | findMin (T (a, _, _)) = findMin a

  fun deleteMin E = raise EMPTY
    | deleteMin (T (E, x, b)) = b
    | deleteMin (T (T (E, _, b), y, c)) = T (b, y, c)
    | deleteMin (T (T (a, x, b), y, c)) = T (T (deleteMin a, x, b), y, c)

  fun inOrder (E, tail) = tail
    | inOrder (T (a, x, b), tail) =
        let val tail' = inOrder (b, tail)
        in inOrder (a, x :: tail') end

  fun treeSort xs =
      let val t = foldl insert empty xs
      in inOrder (t, []) end
end
