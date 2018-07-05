let merge a p q r =
  let n1 = ref (q - p + 1) and n2 = ref (r - q) in
    let la = Array.make !n1 max_int and ra = Array.make !n2 max_int in
      for i = 0 to !n1 - 1 do
        la.(i) <- a.(p + i - 2)
      done;
      for j = 0 to !n2 - 1 do
        ra.(j) <- a.(q + j - 1)
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

