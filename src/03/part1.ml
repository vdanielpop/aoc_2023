type position = {x: int; y: int };;
type number = {value: string; position: position};;

let append_nr nr row col list =
  {value = nr; position = {x = row; y = col}} :: list
in

let append_sym row col list =
  {x = row; y = col} :: list
in

let merge_tuple (toN, toS) (fromN, fromS) =
  (toN @ fromN, toS @ fromS)
in

let process_line (numbers, symbols) line row =
  let rec do_process line numbers symbols buffer pos row =
    match true with
    | _ when pos == String.length line && String.length buffer == 0 -> (numbers, symbols)
    | _ when pos == String.length line && String.length buffer != 0 -> (
      let numbers = append_nr buffer row (pos - String.length buffer) numbers in
      (numbers, symbols)
    )
    | _ when line.[pos] == '.' && String.length buffer == 0 -> do_process line numbers symbols buffer (pos+1) row
    | _ when line.[pos] == '.' && String.length buffer != 0 -> (
      let numbers = append_nr buffer row (pos - String.length buffer) numbers in
      do_process line numbers symbols "" (pos+1) row
    )
    | _ when Utils.is_digit line.[pos] && String.length buffer == 0 -> (
      let buffer = String.make 1 line.[pos] in
      do_process line numbers symbols buffer (pos+1) row
    )
    | _ when Utils.is_digit line.[pos] && String.length buffer != 0 -> (
      let buffer = buffer ^ (String.make 1 line.[pos]) in
      do_process line numbers symbols buffer (pos+1) row
    )
    | _ when String.length buffer == 0 -> do_process line numbers (append_sym row pos symbols) buffer (pos+1) row
    | _ -> (
      let numbers = append_nr buffer row (pos - String.length buffer) numbers in
      let symbols = append_sym row pos symbols in
      do_process line numbers symbols "" (pos+1) row
      )
  in

  do_process line numbers symbols "" 0 row
in

let rec process_lines tuple lines row =
  match lines with
  | [] -> tuple
  | hd :: [] -> merge_tuple tuple (process_line tuple hd row)
  | hd :: tl -> merge_tuple (process_line tuple hd row) (process_lines tuple tl (row + 1))
in

let are_adjacent n s =
  match true with
  | _ when Stdlib.abs (n.position.x - s.x) > 1 -> false
  | _ when n.position.y - 1 <= s.y && n.position.y + String.length n.value >= s.y -> true
  | _ -> false
in

let rec get_valid_values ss n =
  match ss with
  | [] -> 0
  | hd :: tail -> if are_adjacent n hd then int_of_string n.value else get_valid_values tail n
in

let filename = "src/03/in.txt" in
let lines = Utils.read_lines_from_file filename in
let (numbers, symbols) = process_lines ([], []) lines 0 in
  (* List.map (fun n -> print_endline ((string_of_int n.position.x) ^ "," ^ (string_of_int n.position.y) ^ ":" ^ n.value)) numbers; *)
let values = List.map (get_valid_values symbols) numbers in
let sum = List.fold_left (fun x y -> x + y) 0 values in
  print_endline (string_of_int sum);
