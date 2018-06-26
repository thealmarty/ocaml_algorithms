(* Selection sort algorithm written in imperative style, solving exercise 2.2-2 on p.29 of the book "Introduction to Algorithms". Run in toplevel.*)
let selection_sort a =
    for j = 0 to Array.length a -1 do 
    let tmp = ref 0 in
      let themin = ref j and i = ref (Array.length a - 1) in
        while !i >= j && a.(!i) < a.(!themin) do
          themin := !i ;
          i := !i - 1
        done;
      tmp := a.(!themin) ;
      a.(!themin) <- a.(j) ;
      a.(j) <- !tmp ;
    done
;; 
