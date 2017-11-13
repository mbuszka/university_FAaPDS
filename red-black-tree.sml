use "set.sml";
use "ordered.sml";

functor Set (Element: ORDERED) : SET =
struct
  type Elem = Element.T
  datatype Color = R | B
  datatype Tree = E | T of Color * Tree * Elem * Tree
  type Set = Tree

  val empty = E

  fun member (x, E) = false
    | member (x, T (_, a, y, b)) =
      if Element.lt (x, y)
      then member (x, a)
      else if Element.lt (y, x)
        then member (x, b)
        else true

  fun balance ( (B, T (R, T (R, a, x, b), y, c), z, d)
              | (B, T (R, a, x, T (R, b, y, c)), z, d)
              | (B, a, x, T (R, T (R, b, y, c), z, d))
              | (B, a, x, T (R, b, y, T (R, c, z, d))) ) =
                  T (R,T (B,a,x,fc),y,T (B,c,z,d))
    | balance body = T body

  fun insert (x, s) =
      let fun ins E = T (R, E, x, E)
            | ins (s as T (color, a, y, b)) =
              if Element.lt (x, y)
              then balance (color, ins a, y, b)
              else if Element.lt (y, x)
                then balance (color, a, y, ins b)
                else s
          val T (_, a, y, b) = ins s (* guaranteed to be non-empty *)
      in T (B, a, y, b) end

  datatype Digit = One of Elem * Tree
                 | Two of Elem * Tree * Elem * Tree

  (* fun fromOrdList xs =
      let
        fun incr (x as One (a, t), []) = [x]
          | incr (One (a1, t1), One (a2, t2)::rest) = Two (a1, t1, a2, t2) :: rest
          | incr (One (a1, t1), Two (a2, t2, a3, t3) :: rest) = One (a1, t1) *)
end
