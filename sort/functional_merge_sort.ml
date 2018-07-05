(*merge the given sorted lists l and m into one list.*)
let rec compare l m =
  if List.tl l <> [] && List.tl m <> [] then
    (if List.hd l < List.hd m then 
      List.hd l :: compare (List.tl l) m
    else 
      List.hd m :: compare l (List.tl m))
  else if List.tl l = [] then
    (if List.hd l <= List.hd m then
      List.hd l :: m
    else List.hd m :: compare l (List.tl m))
  else if List.hd m <= List.hd l then
    List.hd m :: l
  else List.hd l :: compare m (List.tl l)


