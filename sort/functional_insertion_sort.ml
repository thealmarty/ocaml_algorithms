(* sort l takes a list and returns a sorted list that orders elements from the smallest to the biggest. *)
let rec sort l = 
  (* insert takes a sorted list s and an element h and returns a sorted list that orders elements from the smallest to the biggest. *)
  let rec insert s h =
  match s with
    shd :: stl -> 
      if shd > h then
       h :: shd :: stl
      else
       shd :: insert stl h
    |_ -> [h]
  in
  match l with 
    [] -> []
    | hd :: tl -> insert (sort tl ) hd
