datatype tree = node of int * tree * tree | leaf

exception Unused

fun build (n, x, []) = raise Unused
  | build (0, x, ts) = ts
  | build (n, x, all as t::ts) = build (n - 1, x, node (x, t, t) :: all)

fun log2_ (acc, n) = if n = 0 then acc else log2_ (acc + 1, n div 2)
fun log2 n = log2_ (~1, n)

fun pow2_ (acc, n) = if n = 0 then acc else pow2_ (acc * 2, n - 1)
fun pow2 n = pow2_ (1, n)

fun cbt (n, x) = List.hd (build (n, x, [ leaf ]))

(* 
  given a list of full binary trees containing at most n nodes, with decreasing
  height, this function will first check whether its left or right subtree
  will be full, take the first tree from list and then recurse on other subtree
*)

fun aux (x, 0, 0, [ leaf ]) = leaf
  | aux (x, bt_h, n, t::ts) = 
    let
      val bt_size = pow2 bt_h - 1
    in
      if (bt_size = n) 
        then t
        else if n - bt_size > bt_size div 2
          then node (x, t, aux (x, bt_h - 1, n - bt_size - 1, ts))
          else node (x, aux (x, bt_h - 1, n - bt_size div 2 - 1, ts), List.hd ts)
    end

fun bt (n, x) =
  let 
    val h = log2 (n + 1)
    val trees = build (h, x, [ leaf ])
  in
    aux (x, h, n, trees)
  end
