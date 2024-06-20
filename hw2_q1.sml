(* 1. ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)
fun sig1 f g x = g (f x)

(* 2. 'a -> ('a -> 'a) -> 'a *)
fun sig2 x f = f x

(* 3. ('a * 'b -> 'c) -> 'a -> 'b -> 'c *)
fun sig3 f x y = f (x, y)

(* 4. real -> real -> bool *)
fun sig4 x y = x < y

(* 5. ('a -> 'b) -> 'a list list -> 'b list list *)
fun sig5 f xss = List.map (List.map f) xss

(* 6. bool -> 'a -> 'a -> 'a *)
fun sig6 b x y = if b then x else y

(* 7. unit -> int *)
fun sig7 () = 0

(* 8. ('a -> 'b) -> ('b -> 'c) -> 'a -> 'd -> 'c *)
fun sig8 f g x _ = g (f x)

(* 9. 'a list -> int *)
fun sig9 xs = List.length xs

(* 10. ('a -> 'b) -> 'a list -> 'b list *)
fun sig10 f xs = List.map f xs
