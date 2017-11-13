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
  type 'a t = { mutable state: 'a susp_state }

  let delay f = { state = Thunk f }
  let force r =
    match r.state with
    | Thunk f -> let x = f () in r.state <- Value x; x
    | Value x -> x
end
