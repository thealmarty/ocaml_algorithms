(* Selection sort algorithm written in imperative style, solving exercise 2.2-2 on p.29 of the book "Introduction to Algorithms". Run in toplevel.*)
let selection_sort a =
    for j = 0 to Array.length a -1 do 
    let tmp = ref 0 in
      let themin = ref j in
        for i = j to Array.length a - 1 do
          while a.(i) < a.(!themin) do
            themin := i ;
          done;
        done;
      tmp := a.(!themin) ;
      a.(!themin) <- a.(j) ;
      a.(j) <- !tmp ;
    done
;; 
