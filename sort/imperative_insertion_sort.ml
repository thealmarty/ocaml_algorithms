(* Insertion sort algorithm written in imperative style, strictly following the psuedocode on p.18 of the book "Introduction to Algorithms". Run in toplevel.*)
let insertion_sort a =
  for j = 1 to Array.length a - 1 do
    let key = a.(j) and i = ref (j - 1) in
      while !i >= 0 && a.(!i) > key do
        a.(!i+1) <- a.(!i);
        i := !i - 1
      done;
    a.(!i+1) <- key
  done
;;


    
