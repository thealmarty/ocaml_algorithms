(*merge the given sorted lists l and m into one list.*)
let rec merge l m =
  if l = [] then
    m
  else if m = [] then
    l
  else if List.tl l <> [] && List.tl m <> [] then
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
;;

(*spliteven takes the even elements from the list l.*) 
let rec spliteven l =
  match l with
    hd :: h2 :: tl -> 
      h2 :: spliteven tl
    |[_] -> []
    |_ -> l
;;

(*splitodd takes the odd elements from the list l.*) 
let rec splitodd l =
  match l with
    hd :: h2 :: tl -> 
      hd :: splitodd tl
    |_ -> l
;;

(*merge_sort takes a list l and sort the elements from small to large using the merge sort algorithm.*)
let rec merge_sort l =
  match l with
    hd :: tl -> merge (merge_sort (splitodd l)) (merge_sort (spliteven l))
    |_ -> []
;;
