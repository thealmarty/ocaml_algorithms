let rec select s h =
  match s with
    shd :: stl ->
      (match stl with 
        stlhd :: stltl ->
          if shd < h then
            shd :: h :: select (List.tl stl) (List.hd stl)
          else
            h :: shd :: select (List.tl stl) (List.hd stl)
        |_ ->
            if shd < h then
              shd :: [h]
            else 
              h :: s )
    |_ -> []

