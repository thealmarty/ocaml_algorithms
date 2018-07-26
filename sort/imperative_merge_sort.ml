(* Merge is written according to the pseudocode in "Introduction to Algorithms" 3rd edition p.31. 
 * a is the array.
 * Counting from 1:
   * p is the first element of the left array
   * q is the first element of the right array
   * r is the last element of the right array.*)
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

(* merge_sort is written following the pseudocode on p.34 of the same book, using merge as a subroutine. a is the array, p = 1 (first element) r = length of array. *)
let rec merge_sort a p r =
  if p < r then ( 
    merge_sort a p ((p + r)/2);
    merge_sort a ((p + r)/2 + 1) r;
    merge a p ((p + r)/2) r
  )
;;
