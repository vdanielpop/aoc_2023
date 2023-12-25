let get_prediction values = 
  let rec next_values values = 
    match values with
    | [] -> failwith "Impossible to get here"
    | _ :: [] -> []
    | hd :: tl -> 
      -(hd - (List.hd tl)) :: next_values tl
  in

  let rec is_full_of_zeros list = 
    match list with
    | 0::[] -> true
    | 0::tl -> is_full_of_zeros tl
    | _ -> false
  in

  let rec do_get values = 
    match values with
    | [] -> []
    | hd::tl ->
      match true with 
      | _ when is_full_of_zeros (hd::tl) -> []
      | _ -> hd :: do_get (next_values (hd::tl))
  in

  let rec merge_prediction_parts list i = 
    match list with
    | [] -> 0
    | hd::tl -> i*hd + merge_prediction_parts tl (-1*i)
  in 

  let parts = do_get values in
  merge_prediction_parts parts 1
in

let filename = "src/09/in.txt" in
let lines = Utils.read_lines_from_file filename in
let lists = List.map (fun x -> String.split_on_char ' ' x |> List.map int_of_string) lines in
let predictions = List.map get_prediction lists in
let res = List.fold_left (fun x y -> x+y) 0 predictions in
  string_of_int res |> print_endline
