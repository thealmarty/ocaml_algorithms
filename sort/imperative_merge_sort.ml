(* Merge is written according to the pseudocode in "Introduction to Algorithms" 3rd edition p.31. *)
let merge a p q r =
  let n1 = ref (q - p + 1) and n2 = ref (r - q) in
    let la = Array.make (!n1 + 1) max_int and ra = Array.make (!n2 + 1) max_int in
      for i = 0 to !n1 - 1 do
        la.(i) <- a.(p + i - 1)
      done;
      for j = 0 to !n2 - 1 do
        ra.(j) <- a.(q + j)
      done;
        let i = ref 0 and j = ref 0 in
          for k = p - 1 to r - 1 do
            if la.(!i) <= ra.(!j) then
              (a.(k) <- la.(!i) ;
              i := !i + 1)  
            else 
              (a.(k) <- ra.(!j) ; 
              j := !j + 1) 
          done
;;

(* merge_sort is written following the pseudocode on p.34 of the same book, using merge as a subroutine. *)
let rec merge_sort a p r =
  let q = ref 0 in
    while p < r do
      q := (p + r)/2;
      merge_sort a p q;
      merge_sort a (q + 1) r;
      merge a p q r
    done
;;
