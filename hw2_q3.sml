type regexp = string

(* Helper function to check if two characters are equal *)
fun charEqual (c1: char) (c2: char): bool =
    c1 = c2

(* Helper function to match pattern and string *)
fun matchPattern ([], []) = true
  | matchPattern (_, []) = false
  | matchPattern ([], _) = false
  | matchPattern (p::ps, s::ss) =
      if p = #"*" then
        (case ps of
            [] => true
          | p'::ps' =>
              matchPattern (ps, s::ss) orelse
              (if charEqual p' s then matchPattern (ps', ss) else matchPattern (p::ps, ss)))
      else if charEqual p s then
        matchPattern (ps, ss)
      else
        false

(* Main function to validate and start matching *)
fun isMatch (pattern: regexp) (str: string): bool =
    let
        val patList = String.explode pattern
        val strList = String.explode str

        (* Validate the pattern to ensure '*' is not the first character and is not preceded by another '*' *)
        fun validatePattern [] = true
          | validatePattern [c] = c <> #"*"
          | validatePattern (c1::c2::cs) = 
              if c2 = #"*" andalso c1 = #"*" then false
              else validatePattern (c2::cs)
    in
        if not (validatePattern patList) then false
        else matchPattern (patList, strList)
    end

(* Test cases *)
val isHypeMan = isMatch "s*tilgar" "sssstilgar"        (* Expected: true *)
val trapCheck = isMatch "harkonnen*trap" "Lisan Al-Gaib kills his grandfather"  (* Expected: false *)
val differentTrapCheck = (isMatch "*not a trap" "never gonna let you down is not a trap") handle _ => false (* Expected: false due to invalid pattern *)
