(* select takes a list s and an element h and returns a list that is either
 * unchanged, if h is smaller than or equal to the smallest element of h, or
 * with the smallest element of s replaced by h.  
 * Either h or the smallest element of s is then the input to the function sort.*)
let rec select s h =
  match s with
    shd :: stl ->
      (match stl with 
        stlhd :: stltl ->
          if shd < h then
            h :: select stl shd
          else
            shd :: select stl h
        |_ ->
            if shd < h then
              [h] 
            else 
              [shd]
      )
    |_ -> []
;; 

let rec findmin l = 
  match l with
    shd :: stl ->
      (match stl with 
        stlhd :: stltl ->
          if shd < stlhd then
            select stl shd
          else
            select stl stlhd
        |_ ->
            if shd < List.hd stl then
              shd :: findmin (select stl shd)
            else 
              List.hd stl :: findmin (select stl shd)
      )
    |_ -> []
;; 



