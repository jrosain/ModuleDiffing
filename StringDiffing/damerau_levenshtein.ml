let display_matrix (m: int array array) : unit =
  Array.iteri (fun i l ->
    Array.iteri (fun j e ->
      Printf.printf "%d " e;
    ) l;
    Printf.printf "\n";
  ) m

let diffing (str1: string) (str2: string) (damerau: bool) : int =
  let n1 = String.length str1 in
  let n2 = String.length str2 in
  let t = Array.make_matrix (n1 + 1) (n2 + 1) 0 in

  (* Init array *)
  Array.iteri (fun i _ -> t.(i).(0) <- i) t;
  Array.iteri (fun j _ -> t.(0).(j) <- j) t.(0);

  Array.iteri (fun i l ->
    Array.iteri (fun j _ ->
      if i != 0 && j != 0 then
        (* Classical Levenshtein *)
        let c1 = String.get str1 (i-1) in
        let c2 = String.get str2 (j-1) in
        let cost_subst = 
          if c1 == c2 then 0 else 1
        in
        let del    = t.(i-1).(j  ) + 1 in
        let insert = t.(i  ).(j-1) + 1 in
        let subst  = t.(i-1).(j-1) + cost_subst in
        t.(i).(j) <- min (min del insert) subst;

        (* Damerau's transposition *)
        if damerau && i != 1 && j != 1 then
          let c3 = String.get str1 (i-2) in
          let c4 = String.get str2 (j-2) in
          if c3 == c2 && c1 == c4 then
            let trans = t.(i-2).(j-2) + cost_subst in
            t.(i).(j) <- min t.(i).(j) trans
    ) l
  ) t;
  (* display_matrix t; *)
  t.(n1).(n2)
  