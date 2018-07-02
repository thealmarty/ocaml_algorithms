(* functional_selection_sort_improved more closely follow the selection sort algorithm than functional_selection_sort. *)

(* findmin takes a list and returns the smallest element of the list.*)
let rec findmin (l : 'a list) : 'a =
  match l with
    hd :: h2 :: tl ->
      if hd < h2 then
        findmin (hd :: tl)
      else
        findmin (h2 :: tl)
    |hd :: tl ->
      hd 
;; 

(* remaining_list takes a list and returns a list with the smallest element of the origin list taken out.*)
let rec remaining_list (l : 'a list) : 'a list =
  match l with
    hd :: h2 :: tl ->
      if hd < h2 then
        h2 :: remaining_list (hd :: tl)
      else
        hd :: remaining_list (h2 :: tl)
    |_ -> []
;; 

(* selection_sort takes a list and sort the list using the selection sort algorithm.*)
let selection_sort (l : 'a list) : 'a list =
  match l with
    hd :: tl ->
      findmin l :: selection_sort (remaining_list l)
    |[] -> l
;;
