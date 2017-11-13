signature SET =
sig
  type Elem
  type Set

  val empty : Set
  val fromOrdList : Elem list -> Set
  val member : Elem * Set -> bool
  val insert : Elem * Set -> Set
end
