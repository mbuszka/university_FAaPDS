fun mergesort [] = []
	| mergesort [x] = [x]
	| mergesort ls =
	let
		fun split (acc1, acc2, x::y::xs) = split (x::acc1, y::acc2, xs)
			| split (acc1, acc2, [x]) = (x::acc1, acc2)
			| split (acc1, acc2, []) = (acc1, acc2)

		fun merge (x::xs, y::ys) = if (x < y) 
			then x :: merge (xs, y::ys) else y :: merge (x::xs, ys)
			| merge ([], ys) = ys
			| merge (xs, []) = xs

		val (xs, ys) = split ([], [], ls)
	in
		merge (mergesort xs, mergesort ys)
	end
