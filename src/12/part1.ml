type record = { str : string; len : int; list : int list };;

let valid_for_group c = match c with '#' | '?' -> true | _ -> false in
let valid_for_space c = match c with '.' | '?' -> true | _ -> false in

let can_start_with_group str start n =
  let rec check str i =
    match i with
    | 0 -> true
    | _ ->
    match valid_for_group str.[start + n - i] with
    | true -> check str (i - 1)
    | false -> false
  in

  check str n && valid_for_space str.[start + n]
in

let can_be_group str start n =
  let rec check str i =
    match i with
    | 0 -> true
    | _ ->
    match valid_for_group str.[start + n - i] with
    | true -> check str (i - 1)
    | false -> false
  in

  check str n
in

let get_arrangements { str; len; list } =
  let visited = Hashtbl.create len in
  let hashtbl_has h k =
    match Hashtbl.find_opt h k with Some _ -> true | None -> false
  in

  let is_result_valid hash_key remaining =
    String.contains remaining '#' = false
    && hashtbl_has visited hash_key = false
  in

  let rec do_get start list hash_key =
    let nlen = len - start in

    match list with
    | [] ->
      if is_result_valid hash_key (String.sub str start nlen) then
        let _ = Hashtbl.add visited hash_key "1" in
        1
      else 0
    | hd :: tl ->
    match true with
    | _ when nlen < hd -> 0
    | _ when nlen = hd ->
      if can_be_group str start hd then
        let hash_key = hash_key ^ "-" ^ string_of_int start in
        do_get (start + hd) tl hash_key
      else 0
    | _ ->
      if can_start_with_group str start hd then
        let new_key = hash_key ^ "-" ^ string_of_int start in
        if str.[start] = '#' then do_get (start + hd + 1) tl new_key
        else
          do_get (start + hd + 1) tl new_key
          + do_get (start + 1) (hd :: tl) hash_key
      else if Mystring.has_chars_in_order (String.sub str start hd) [ '#'; '.' ]
      then 0
      else if str.[start] = '#' then 0
      else do_get (start + 1) (hd :: tl) hash_key
  in

  do_get 0 list ""
in

let rec parse_input lines =
  match lines with
  | [] -> []
  | hd :: tl -> (
    let parts = String.split_on_char ' ' hd in
    match parts with
    | [ str; list ] ->
      let list =
        String.split_on_char ',' list |> List.map (fun x -> int_of_string x)
      in
      { str; list; len = String.length str } :: parse_input tl
    | _ -> failwith "Impossible to get here")
in

let filename = "src/12/in.txt" in
let lines = Utils.read_lines_from_file filename in
let records = parse_input lines in
let arrangements = List.map get_arrangements records in
let res = List.fold_left (fun acc x -> acc + x) 0 arrangements in
string_of_int res |> print_endline
