module type MyLazy =
sig
  type 'a t

  val delay: (unit -> 'a) -> 'a t
  val force: 'a t -> 'a
end

module MyLazy: MyLazy =
struct
  type 'a susp_state = Thunk of (unit -> 'a)
                     | Value of 'a
  type 'a t = 'a susp_state ref

  let delay f = ref (Thunk f)
  let force r =
    match !r with
    | Thunk f -> let x = f () in r := Value x; x
    | Value x -> x
end
