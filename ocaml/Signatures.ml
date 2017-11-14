module type DEQUE =
sig
  type 'a t

  val empty: 'a t
  val isEmpty: 'a t -> bool
  val cons: 'a -> 'a t -> 'a t
  val head: 'a t -> 'a
  val tail: 'a t -> 'a t
  val snoc: 'a t -> 'a -> 'a t
  val daeh: 'a t -> 'a
  val liat: 'a t -> 'a t
end
