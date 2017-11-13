open LzyStream

type 'a tree_node = Node of 'a * 'a tree stream
                  | Leaf
and 'a tree = 'a tree_node Lazy.t

let constTree x =
  let rec t = lazy ( Node (x, s))
  and s = lazy (LzyStream.Cons (t, s))
  in t

let rec tMap f t = lazy (
  match t with
  | lazy (Node (x, ts)) -> Node (f x, LzyStream.map (tMap f) ts)
  | lazy Leaf -> Leaf
)

exception NoSuchTree

let subTree n t =
  match t with
  | (lazy (Node (_, ts))) ->
    (match LzyStream.drop n ts with
    | lazy LzyStream.Nil -> raise NoSuchTree
    | lazy (LzyStream.Cons (t, _)) -> t)
  | (lazy Leaf) -> raise NoSuchTree
