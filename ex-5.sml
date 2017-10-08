fun nc_sort (z::zs) =
    let
      fun find_smallest (l, p, x::xs) = if x < p andalso x > l
            then find_smallest(l, x, xs)
            else find_smallest(l, p, xs)
        | find_smallest (_, p, []) = p
      
      fun count_elems (e, y::ys, acc) = if e = y
            then count_elems (e, ys, acc + 1)
            else count_elems (e, ys, acc)
        | count_elems (_, [], acc) = acc

      fun prepend (0, _, xs) = xs
        | prepend (n, e, xs) = e :: prepend (n - 1, e, xs)

      fun max (x::xs, prev) = if x < prev then max (xs, prev) else max (xs, x)
        | max ([], prev) = prev

      fun min (x::xs, prev) = if x > prev then min (xs, prev) else min (xs, x)
        | min ([], prev) = prev

      fun aux (x::xs, limit) =
          let
            val m = max(xs, x)
            val e = find_smallest (limit, m, x::xs)
            val n = count_elems (e, x::xs, 0)
            val tl = if m = e then [] else aux (x::xs, e)
          in 
            prepend (n, e, tl)
          end
        | aux ([], _) = []
      val mn = min (zs, z)
    in
      aux (z::zs, mn - 1)
    end
  | nc_sort [] = []
