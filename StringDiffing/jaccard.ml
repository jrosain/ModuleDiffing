
module SS = Set.Make(String)

(** Decomposes the string `str` into |str| - granularity strings in a set.
    For instance, if the granularity is 2 and the string is abcde, then this function returns the 
    following set:
      { ab, bc, cd, de }
    
    If the granularity is greater than the length of the string, then it fails. *)
let decompose (str: string) (granularity: int) =
  if (String.length str) < granularity then failwith ("String '" ^ str ^ "' does not contain any " ^
                                                        "granularity of size " ^
                                                          (string_of_int granularity))
                                                                       
  else
    let rec aux (pos: int) acc =
      if pos = (String.length str) - granularity + 1 then acc
      else
        aux (pos + 1) (SS.add (String.sub str pos granularity) acc)
    in aux 0 (SS.empty)

(** Returns jaccard's similarity between `str1` and `str2` using a given granularity. *)
let diffing (str1: string) (str2: string) (granularity: int) : float =
  let set_str1 = decompose str1 granularity in
  let set_str2 = decompose str2 granularity in
  (float_of_int (SS.cardinal (SS.inter set_str1 set_str2)) /.
    float_of_int (SS.cardinal (SS.union set_str1 set_str2)))
