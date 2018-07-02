(* select takes a list s and returns a list with all the same elements of s but the smallest element of s is the last element of the list.*)
let rec select s =
  match s with
    shd :: sh2 :: stl ->
      if shd < sh2 then
        sh2 :: select (shd :: stl)
      else
        shd :: select (sh2 :: stl)
    |shd :: stl ->
      [shd] 
    |_ -> []
;; 



