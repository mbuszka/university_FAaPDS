use "finite-map.sml";

(* Exercise 20 *)
functor UnbalancedMap (Elem: ORDERED): FiniteMap =
struct
  type Key = Elem.T
  datatype 'a Map = E | N of Key * 'a * 'a Map * 'a Map

  val empty = E

  fun bind (k, v, E) = N (k, v, E, E)
    | bind (k, v, N (k1, v1, t1, t2)) =
      if Elem.leq (k, k1)
      then if Elem.leq (k1, k)
        then N (k, v, t1, t2)
        else N (k1, v1, bind (k, v, t1), t2)
      else N (k1, v1, t1, bind (k, v, t2))

  fun lookup (k, E) = raise NotFound
    | lookup (k, N (k1, v1, t1, t2)) =
      if Elem.leq (k, k1)
      then if Elem.leq (k1, k)
        then v1
        else lookup (k, t1)
      else lookup (k, t2)
end
