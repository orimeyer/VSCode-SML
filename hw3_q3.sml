fun tokenize x =
   String.tokens (fn c => c = #" ")
       (String.translate (fn #"(" => "( " | #")" => " )" | c => str c) x);

(* Define the datatype for Atoms *)
datatype Atom =
   NIL 
 | SYMBOL of string;

(* Define the datatype for S-Expressions *)
datatype SExp =
   ATOM of Atom 
 | CONS of (SExp * SExp);

(* Helper function to parse a string into an Atom *)
fun parseAtom s =
  if s = "NIL" then ATOM NIL
  else ATOM (SYMBOL s);

(* Function to parse a list of strings into an SExp *)
fun parse [] = ATOM NIL
  | parse ("("::rest) = 
      let
          val (list, rest') = parseElements rest
      in
          CONS (list, parse rest')
      end
  | parse (")"::rest) = parse rest
  | parse (x::rest) = CONS (parseAtom x, parse rest)

and parseElements [] = (ATOM NIL, [])
  | parseElements (")"::rest) = (ATOM NIL, rest)
  | parseElements tokens =
      let
          val (element, rest) = parseElement tokens
          val (elements, rest') = parseElements rest
      in
          (CONS (element, elements), rest')
      end

and parseElement ("("::rest) = 
      let
          val (list, rest') = parseElements rest
      in
          (CONS (list, ATOM NIL), rest')
      end
  | parseElement (x::rest) = (parseAtom x, rest);


(* Examples *)
val result1 = parse ["a"];
val result2 = parse ["(","+","2","3",")"];
val result3 = parse ["(","+","1","(","+","1","3",")",")"]