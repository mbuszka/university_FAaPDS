use "heap.sml";

functor SizedHeap (Heap : HEAP): HEAP =
struct
  structure H = Heap
  structure Elem = Heap.Elem
  type Heap = int * H.Heap

  val empty = (0, H.empty)
  fun isEmpty (0, _) = true
    | isEmpty _      = false

  fun insert (x, (n, h)) = (n+1, H.insert (x, h))
  fun findMin (_, h) = H.findMin h
  fun deleteMin (n, h) = (n-1, H.deleteMin h)
end
