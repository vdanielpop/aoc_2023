let get list i = 
  let rec do_get list a = 
    match a = i with
    | true -> List.hd list
    | false -> do_get (List.tl list) (a+1)
  in

  do_get list 0
