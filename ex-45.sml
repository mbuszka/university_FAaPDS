use "queue.sml";
use "stream.sml";

open Stream;
infix ++;


structure BankersQueue: QUEUE =
struct
  type 'a Queue = int * 'a Stream * int * 'a Stream

  val empty = (0, $Nil, 0, $Nil)
  fun isEmpty (lenf, _, _, _) = lenf = 0

  fun check (q as (lenf, f, lenr, r)) =
      if lenr > 2 * lenf then (lenf + lenr, f ++ (reverse r), 0, $Nil) else q

  fun snoc ((lenf, f, lenr, r), x) = check (lenf, f, lenr + 1, $(Cons (x, r)))

  exception EMPTY

  fun tail (_, $Nil, _, _) = raise EMPTY
    | tail (lenf, $(Cons (_, f)), lenr, r) = check(lenf - 1, f, lenr, r)

  fun head (_, $Nil, _, _) = raise EMPTY
    | head (_, $(Cons (x, _)), _, _) = x
end

fun gen (0, acc) = acc
  | gen (n, acc) = gen (n - 1, n :: acc)

open BankersQueue;
open Timer;

fun test () =
  let val l = gen (1000000, [])
      val timer = startCPUTimer ()
      val q = foldl (fn (x, q) => snoc (q, x)) empty l
      fun deplete q = if isEmpty q then () else deplete (tail q)
      val _ = deplete q
      val time = checkCPUTimer timer
  in time end
