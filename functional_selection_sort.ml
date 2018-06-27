let rec sort l =
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
      in
      match l with
        [] -> []
        | hd :: tl -> 
          match tl with
            [] -> []
        |_ ->
          let rec findmin n =
              if List.nth tl n = List.nth (select tl hd) n then
                (if n < List.length tl - 1 then findmin (n + 1)
                else sort tl)
              else 
                List.nth tl :: sort tl
            in
            findmin (0)
;;
