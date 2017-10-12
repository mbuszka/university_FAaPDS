datatype tree = node of int * tree * tree | leaf

val example = node (5, 
  node (2, node (1, leaf, leaf), node (4, leaf, leaf)),
  node (9, node (7, leaf, leaf), node (11, leaf, leaf)))

fun naive_member (x, node (y, t1, t2)) =
      if x <= y
        then if y <= x
          then true
          else naive_member (x, t1)
        else naive_member (x, t2)
  | naive_member (x, leaf) = false

fun naive_insert (x, t as node (y, t1, t2)) =
      if x <= y
        then if y <= x
          then t
          else node (y, naive_insert (x, t1), t2)
        else node (y, t1, naive_insert (x, t2))
  | naive_insert (x, leaf) = node (x, leaf, leaf)

(* 1. Better member which performs at most h + 1 comparisons *)

fun member (x, t) = 
    let
      fun aux (last_left, node (y, t1, t2)) =
            if x <= y
              then aux (SOME y, t1)
              else aux (last_left, t2)
        | aux (SOME l, leaf) = l <= x
        | aux (NONE, leaf) = false
    in 
      aux (NONE, t)
    end

(* 3. Better insert which does not unnecessarily copy tree along the path *)

fun insert (x, t) =
    let
      fun aux (last_left, node (y, t1, t2)) =
            if x <= y
              then Option.map (fn tl => node (y, tl, t2)) (aux (SOME y, t1))
              else Option.map (fn tr => node (y, t1, tr)) (aux (last_left, t2))
        | aux (SOME l, leaf) =
            if l <= x
              then NONE
              else SOME (node (x, leaf, leaf))
        | aux (NONE, leaf) = 
            SOME (node (x, leaf, leaf))
    in
      case aux (NONE, t) of
          SOME tree => tree
        | NONE => t
    end

