use "stream.sml";
use "queue.sml";

open Stream;
infix ++;

structure RealTimeQueue =
struct 
  type 'a Queue = 'a Stream * 'a list * 'a Stream

  val empty = ($Nil, [], $Nil)
  fun isEmpty ($Nil, _, _) = true
    | isEmpty _ = false

  fun rotate ($(Cons(x, xs)), y::ys, a) =
        $(Cons (x, rotate(xs, ys, $(Cons(y, a)))))
    | rotate ($Nil, y::_, a) = $(Cons (y, a))

  fun exec (f, r, $(Cons (_, s))) = (f, r, s)
    | exec (f, r, $Nil) = let val f' = rotate (f, r, $Nil) in (f', [], f') end

  fun snoc ((f, r, s), x) = exec (f, x::r, s)

  fun len (_, r, s) = 2 * (length r) + (slength s)

end

