fun to_binary 0 = []
  | to_binary n =
    let
      fun helper 0 acc = acc
        | helper n acc = (n mod 2) :: helper (n div 2) acc
    in
      helper n []
    end;

fun encode vec =
    let
        fun update_and_flip (x, (count1, count0)) =
            if x = 1 then (0, (count1 - 1, count0 + 1))
            else (1, (count1 + 1, count0 - 1))

        fun balance ([], count1, count0, idx) = []
          | balance (x::xs, count1, count0, idx) =
            if count1 = count0 then (x::xs) @ to_binary idx
            else
                let
                    val (flipped, (new_count1, new_count0)) = update_and_flip (x, (count1, count0))
                in
                    flipped :: balance (xs, new_count1, new_count0, idx + 1)
                end

        val count1 = List.foldl (fn (x, acc) => if x = 1 then acc + 1 else acc) 0 vec
        val count0 = List.length vec - count1
    in
        balance (vec, count1, count0, 0)
    end;

fun decode (encoded_vec, original_length) =
    let
        val idx_length = List.length encoded_vec - original_length

        val encoded_data = List.take (encoded_vec, original_length)

        val idx_binary = List.drop (encoded_vec, original_length)

        fun binary_to_decimal bin_list =
            let
                fun helper [] acc _ = acc
                  | helper (x::xs) acc pow =
                    let
                        val new_acc = acc + (x * pow)
                    in
                        helper xs new_acc (pow * 2)
                    end
            in
                helper bin_list 0 1
            end

        val flips_needed = binary_to_decimal idx_binary

        fun decode_helper ([], _) = []
          | decode_helper (x::xs, 0) = x :: decode_helper (xs, 0)
          | decode_helper (x::xs, flips_left) =
            let
                val flipped_bit = if x = 1 then 0 else 1
            in
                flipped_bit :: decode_helper (xs, flips_left - 1)
            end

    in
        decode_helper (encoded_data, flips_needed)
    end;    


val decoded1 = decode ([0, 0, 0, 1, 1, 1, 1, 1], 6);
(* decoded1 should be [1, 1, 1, 1, 1, 1] *)

val decoded2 = decode ([0, 1, 0, 1, 1], 4);
(* decoded2 should be [1, 1, 0, 1] *)

val decoded3 = decode ([0, 1, 0, 0, 1, 1, 0, 0, 1], 6);
(* decoded3 should be [1, 0, 1, 1, 1, 1] *)

