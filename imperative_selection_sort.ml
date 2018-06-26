(* Selection sort algorithm written in imperative style, solving exercise 2.2-2 on p.29 of the book "Introduction to Algorithms". Run in toplevel.*)
let selection_sort a =
  for j = 0 to Array.length a - 1 do
    let min = ref j and i = ref (Array.length a - 1 - j) in
      while !i > 0 && a.(!i) < a.(!min) do
        a.(!i) <- a.(!min) ;
        min := !i ;
        i := !i - 1
      done;
    a.(j) <- a.(!min)
  done
;;


    
