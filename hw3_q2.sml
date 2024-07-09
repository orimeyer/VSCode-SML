datatype ('a, 'b) heterolist = NIL | ::: of 'a * ('b, 'a) heterolist;
infixr 5 :::;

fun build4 (x, one, y, two) = x ::: one ::: y ::: two ::: NIL;

fun unzip NIL = ([], [])
  | unzip (x ::: rest) =
      case rest of
          NIL => ([x], [])
        | (y ::: rest') =>
            let
              val (xs, ys) = unzip rest'
            in
              (x :: xs, y :: ys)
            end;

exception Empty;

fun zip (xs, ys) =
    let
        fun aux ([], []) = NIL
          | aux (x::xs, y::ys) = x ::: y ::: aux (xs, ys)
          | aux _ = raise Empty

    in
        if length xs = length ys then aux (xs, ys)
        else raise Empty
    end;
