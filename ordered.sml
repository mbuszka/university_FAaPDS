signature ORDERED =
sig
  type T

  val leq: T * T -> bool
  val eq: T * T -> bool
  val lt: T * T -> bool
end

structure intOrdered: ORDERED = struct
  type T = int
  val leq = op<=
  val eq = op=
  val lt = op<
end 