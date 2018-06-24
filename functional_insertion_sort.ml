let rec insert s h =
  match s with
    shd :: stl -> if shd > h then
                    h :: shd :: stl
                  else
                    shd :: insert stl h
;;


    
