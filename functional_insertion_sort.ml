(* insert takes a sorted list s and an element h and returns a sorted list that orders elements from the smallest to the biggest. *)
let rec insert s h =
  match s with
    shd :: stl -> if shd > h then
                    h :: shd :: stl
                  else
                    shd :: insert stl h
    |_ -> [h]
;;


