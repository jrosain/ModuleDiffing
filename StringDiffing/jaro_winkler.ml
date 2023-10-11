module StringSet = Set.Make(String)

(*=======*)

(* Utility function to round a float number to 2 decimal places *)
let round_to_2_decimals f =
  let s = Printf.sprintf "%.2f" f in
  float_of_string s

(*=======*)


(**
[matching_characters_count s1 s2] returns the number of matching characters between [s1] and [s2].
The characters are said to be matching if they are the same and the characters are not further than \floor{max(|s1|, |s2|)/2} - 1,
where |s1| and |s2| are the lengths of the strings s1 and s2 respectively.
*)
let matching_characters_count s1 s2 =
  let max_dist = (max (String.length s1) (String.length s2) / 2) - 1 in
  let rec loop i j acc =
    if i >= String.length s1 then acc
    else if j >= String.length s2 then loop (i + 1) 0 acc
    else
      let dist = abs (i - j) in
      if dist > max_dist then loop i (j + 1) acc
      else if s1.[i] = s2.[j] then loop i (j + 1) (acc + 1)
      else loop i (j + 1) acc in
  loop 0 0 0
  

let _ = assert ((matching_characters_count "MARTHA" "MARHTA") = 6);;
let _ = assert ((matching_characters_count "DIXON" "DICKSONX") = 4);;
let _ = assert ((matching_characters_count "DWAYNE" "DUANE") = 4);;

(**
[get_transposition_count s1 s2] calculates the number of transpositions between two strings s1 and s2.
A transposition occurs when two adjacent characters in s1 are in a different order in s2.
For example, in the strings "MARTHA" and "MARHTA", the transposition occurs between 'T' and 'H'.
*)
let rec get_transposition_count s1 s2 =
  if String.length s1 > String.length s2 then 
    get_transposition_count s2 s1
  else
    let trans_len_max = (max (String.length s1) (String.length s2)) / 2 - 1 in
    if trans_len_max <= 0 then 
      0 
    else 
      let rec loop2 i j acc =
        if j > trans_len_max || i + j >= (String.length s1) then 
          acc 
        else
          if s1.[i] == s2.[i + j] && s1.[i + j] == s2.[i] then
            loop2 i (j + 1) (acc + 1) 
          else 
            loop2 i (j + 1) acc in
      let rec loop i j acc = 
        if i >= (String.length s1) then 
          acc 
        else 
          if s1.[i] != s2.[j] then
            let acc2 = loop2 i 1 0 in 
            loop (i + 1) 0 (acc + acc2)
          else 
            loop (i + 1) 0 acc
          in 
      loop 0 0 0
    


let _ = assert ((get_transposition_count "MARTHA" "MARHTA") = 1);;
let _ = assert ((get_transposition_count "DIXON" "DICKSONX") = 0);;
let _ = assert ((get_transposition_count "DWAYNE" "DUANE") = 0);;

(**
[get_jaro_distance s1 s2] calculates the Jaro distance between two strings s1 and s2.
*)
let get_jaro_distance s1 s2 =
  let m = float_of_int (matching_characters_count s1 s2) in
  let t = float_of_int (get_transposition_count s1 s2) in
  let l1 = float_of_int (String.length s1) in
  let l2 = float_of_int (String.length s2) in
  let d = (m /. l1 +. m /. l2 +. (m -. t) /. m) /. 3. in
  d


let _ =
  assert (round_to_2_decimals (get_jaro_distance "MARTHA" "MARHTA") = 0.94);;

let _ = 
  assert (round_to_2_decimals (get_jaro_distance "DIXON" "DICKSONX") = 0.77);;

let _ = 
  assert (round_to_2_decimals (get_jaro_distance "DWAYNE" "DUANE") = 0.82);;


(**
[get_jaro_winkler_distance ?(p = 0.1) s1 s2] calculates the Jaro-Winkler distance between two strings s1 and s2.
Optional parameter p (default value 0.1) is a constant scaling factor for the prefix scale.
*)
let diffing ?(p = 0.1) s1 s2 =
  let jaro_distance = get_jaro_distance s1 s2 in
  let l = min (String.length s1) (String.length s2) in 
  let rec get_max_prefix i =
    (*la taille max doit être 4*)
    if i >= 4 then i
    else if i >= l then i
    else if s1.[i] = s2.[i] then get_max_prefix (i + 1)
    else i in
  let lp = float_of_int (get_max_prefix 0) *. p in
  jaro_distance +. lp *. (1. -. jaro_distance)


let _ = 
  assert (round_to_2_decimals (diffing "MARTHA" "MARHTA") = 0.96);;

let _ =
  assert (round_to_2_decimals (diffing "DIXON" "DICKSONX") = 0.81);;

let _ = 
  assert (round_to_2_decimals (diffing "DWAYNE" "DUANE") = 0.84);;

