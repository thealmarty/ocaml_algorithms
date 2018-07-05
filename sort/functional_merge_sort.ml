(*merge the given sorted lists l and m into one list.*)
let rec merge l m =
  if List.tl l <> [] && List.tl m <> [] then
    (if List.hd l < List.hd m then 
      List.hd l :: merge (List.tl l) m
    else 
      List.hd m :: merge l (List.tl m))
  else if List.tl l = [] then
    (if List.hd l <= List.hd m then
      List.hd l :: m
    else List.hd m :: merge l (List.tl m))
  else if List.hd m <= List.hd l then
    List.hd m :: l
  else List.hd l :: merge m (List.tl l)

let rec spliteven l =
  match l with
    hd :: h2 :: tl -> 
      hd :: splitfirst tl
    |_ -> l

