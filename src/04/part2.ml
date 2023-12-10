module StringSet = Set.Make(String);;

let count_common_elements a b =
  let sa = StringSet.empty in
  let sb = StringSet.empty in
  let sa = List.fold_right StringSet.add a sa in
  let sb = List.fold_right StringSet.add b sb in
  let intersection = StringSet.inter sa sb in
  StringSet.cardinal intersection
in

let increment_extras extras pos current count =
  for i = 1 to count do
    extras.(pos + i) <- extras.(pos + i) + current
  done;
  extras
in

let rec get_points_from_line i lines extras =
    match lines with
    | [] -> 0
    | line :: tl ->
      begin
      let parts = String.split_on_char ':' line in
      let numbers_parts = String.split_on_char '|' (List.hd (List.tl parts)) in
      let count = count_common_elements
        (Mystring.trim_split_on_char ' ' (List.hd (List. tl numbers_parts)))
        (Mystring.trim_split_on_char ' ' (List.hd numbers_parts))
      in

      extras.(i) + get_points_from_line (i+1) tl (increment_extras extras i extras.(i) count)
    end
in

let filename = "src/04/in.txt" in
let lines = Utils.read_lines_from_file filename in
let extras = Array.make (List.length lines) 1 in
let res = get_points_from_line 0 lines extras in
  print_endline (string_of_int res);
