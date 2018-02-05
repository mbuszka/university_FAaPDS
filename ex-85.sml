use "queue.sml";

open Lazy;
open SMLofNJ.Susp;
Control.lazysml := true;


functor BootstrapQueue (PrimQ : QUEUE) =
struct
  type 'a PrimQueue = 'a PrimQ.Queue

  datatype 'a Queue
    = E
    | Q of int * 'a list * 'a list susp PrimQueue * int * 'a list

  val empty = E
  fun isEmpty E = true
    | isEmpty _ = false
  
  fun checkF (q as (lenf, f, m, lenr, r)) = case f of
      [] => if PrimQ.isEmpty m then E
            else Q (lenf, force (PrimQ.head m), PrimQ.tail m, lenr, r)
    | _  => Q q 

  fun checkQ (q as (lenf, f, m, lenr, r)) =
    if lenr <= lenf then checkF q
    else checkF (lenf + lenr, f, PrimQ.snoc (m, delay (fn () => rev r)), 0, [])
  
  fun snoc (E, x) = Q (1, [x], PrimQ.empty, 0, [])
    | snoc (Q (lenf, f, m, lenr, r), x) = checkQ (lenf, f, m, lenr + 1, x::r)
  exception EMPTY;
  fun head E = raise EMPTY
    | head (Q (_, x::_, _, _, _)) = x

  fun tail E = raise EMPTY
    | tail (Q (lenf, _::f, m, lenr, r)) = checkQ (lenf - 1, f, m, lenr, r)
end