(* Part 1 *)

(* Suboptimal solution *)
fun sublist (x::xs) = 
	let
		fun addx (ys::yss) = (x::ys) :: addx yss 
			| addx nil = nil
		val xss = sublist xs
	in (* Here the whole xss gets rebuilt *)
		xss @ addx xss
	end
	| sublist [] = [[]]

(* Better solution *)
fun sublist2 (x::xs) = 
	let
		fun add_merge (zss, ys::yss) = (x::ys) :: add_merge (zss, yss)
			| add_merge (zss, []) = zss
		val xss = sublist2 xs
		in
			add_merge (xss, xss)
		end
	| sublist2 [] = [[]]


(* Part 2 *)

datatype tree = & of int * (tree * tree) | %
infix &

val ex = 4 & (2 & (1 & (%, %), 3 & (%, %)), 5 & (%, %))

(* Suboptimal solution *)
fun flatten (x & (t1, t2)) = flatten t1 @ [x] @ flatten t2
	| flatten % = nil

(* Better solution *)
fun flatten2 t =
	let 
		fun aux (x & (t1, t2), rest) = aux (t1, x :: aux (t2, rest))
			| aux (%, rest) = rest
	in
		aux (t, [])
	end

(* Part 3 *)
fun rev (x::xs) = rev xs @ [x]
	| rev [] = []

fun rev2 ys =
	let
		fun aux (acc, x::xs) = aux (x::acc, xs)
			| aux (acc, []) = acc
	in
		aux ([], ys)
	end
