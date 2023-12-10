module StringSet = Set.Make(String);;

let count_common_elements a b =
  let sa = StringSet.empty in
  let sb = StringSet.empty in
  let sa = List.fold_right StringSet.add a sa in
  let sb = List.fold_right StringSet.add b sb in
  let intersection = StringSet.inter sa sb in
  StringSet.cardinal intersection
in

let get_points_from_line line =
  let parts = String.split_on_char ':' line in
  let numbers_parts = String.split_on_char '|' (List.hd (List.tl parts)) in
  let count = count_common_elements
    (Mystring.trim_split_on_char ' ' (List.hd (List. tl numbers_parts)))
    (Mystring.trim_split_on_char ' ' (List.hd numbers_parts)) in

  match count with
  | 0 -> 0
  | _ -> Utils.pow 2 (count - 1)
in

let filename = "src/04/in.txt" in
let lines = Utils.read_lines_from_file filename in
let points = List.map get_points_from_line lines in
let sum = List.fold_left (fun x y -> x + y) 0 points in
  print_endline (string_of_int sum);
