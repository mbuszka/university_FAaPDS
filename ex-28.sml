
Control.lazysml := true;
open Lazy;

structure BinInfDepthTree =
struct
  datatype lazy 'a tree = Node of 'a tree * 'a * 'a tree
                        | Leaf

  fun constTree x =
      let
        val rec lazy t = Node (t, x, t)
      in t end

  fun tMap f =
      let
        fun lazy mp (Node (l, x, r)) = Node (mp l, f x, mp r)
               | mp Leaf = Leaf
      in
        mp
      end

  datatype dir = L | R

  fun subTree (L::ds) = let fun lazy f (Node (l, _, _)) = subTree ds l
                                  |  f Leaf = Leaf
                        in f end
    | subTree (R::ds) = let fun lazy f (Node (_, _, r)) = subTree ds r
                                  |  f Leaf = Leaf
                        in f end
    | subTree [] = let fun lazy f x = x in f end

  fun lazy mirror (Node (l, x, r)) = Node (r, x, l)
        |  mirror Leaf = Leaf

  fun lazy buildNats x = Node (buildNats (2 * x), x, buildNats (2 * x + 1))
  val nats = buildNats 1
end
