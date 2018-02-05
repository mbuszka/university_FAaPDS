use "stream.sml";

structure RAList =
struct
  datatype 'a RList
    = Nil
    | One of 'a * ('a * 'a) RList susp
    | Two of 'a * 'a * ('a * 'a) RList susp
    | Three of 'a * 'a * 'a * ('a * 'a) RList susp

  val empty = Nil
  fun isEmpty Nil = true
    | isEmpty _   = false

  fun cons (x, Nil) = One (x, $Nil)
    | cons (x, One (y, ys)) = Two (x, y, ys)
    | cons (x, Two (y, z, ys)) = Three (x, y, z, ys)
    | cons (a, Three (b, c, d, xs)) = One (a, )
end