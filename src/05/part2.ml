
type rule = {src_start: int; src_end:int; len: int; dest_start: int;}
type range = {left: int; right: int};;

let sort_rules rules =
  let cmp_rules r1 r2 =
    r1.src_start - r2.src_start
  in

  List.sort cmp_rules rules
in

let sort_ranges ranges =
  let cmp_ranges r1 r2 =
    r1.left - r2.left
  in

  List.sort cmp_ranges ranges
in

let get_initial_ranges line =
  let parts = String.split_on_char ':' line in
  let string_vals = Mystring.trim_split_on_char ' ' (List.hd (List.tl parts)) in
  let int_vals = List.map int_of_string string_vals in

  let rec do_get list =
    match list with
    | _::[] -> raise (Invalid_argument "odd number of seeds")
    | [] -> []
    | (hd1::hd2::tl) -> ({left=hd1; right=(hd1+hd2-1)}::(do_get tl))
  in

  do_get int_vals
in

let map_ranges ranges rules =
  let map_rule {left=left; right=right} rule =
    if right < rule.src_start then
      (None, [{left=left; right=right}])
    else if left < rule.src_start && right > rule.src_end then
      (Some {left=rule.src_end + 1; right=right}, [
        {left=left; right=rule.src_start - 1};
        {left=rule.dest_start; right=rule.dest_start + rule.len - 1}
      ])
    else if left < rule.src_start && right <= rule.src_end then
      (None,[
        {left=left; right=rule.src_start - 1};
        {left = rule.dest_start; right = rule.dest_start + (right - rule.src_start)}
        ])
    else if right <= rule.src_end then
      (None, [{left=rule.dest_start + (left - rule.src_start); right=rule.dest_start + (right - rule.src_start)}])
    else if left < rule.src_end then
      (Some {left=rule.src_end + 1; right=right},
        [{left=rule.dest_start + (left - rule.src_start); right=rule.dest_start + rule.len - 1}])
    else
      (Some {left=left; right=right}, [])
  in

  let rec map_rules rules mapped range =
    match rules with
    | [] -> (range::mapped)
    | hd::tl ->
      let (range, rule_mapped) = map_rule range hd in
      match range with
      | None -> mapped @ rule_mapped
      | Some range -> map_rules tl (mapped @ rule_mapped) range
  in

  (* print_endline "Ranges:";
  List.iter (fun r -> (string_of_int r.left) ^ ":" ^ (string_of_int r.right) |> print_endline) ranges;
  print_newline();
  print_endline "Rules:";
  List.iter (fun r -> (string_of_int r.src_start) ^ ":" ^ (string_of_int r.src_end) ^ " " ^ (string_of_int r.dest_start) ^ ":" ^ (string_of_int (r.dest_start + r.len - 1))|> print_endline) rules;
  print_newline(); *)

  List.fold_left (map_rules rules) [] ranges
in

let add_rule line rules =
  let parts = Mystring.trim_split_on_char ' ' line in
  let int_parts = List.map int_of_string parts in
  match int_parts with
  | dest::src::len::_ -> ({src_start = src; src_end = src + len - 1; len = len; dest_start = dest;}::rules)
  | _ -> raise (Invalid_argument "wrong rule definition")
in

let process_line (ranges, rules) line =
  match true with
  | _ when String.length line = 0 -> ((map_ranges ranges (sort_rules rules)), [])
  | _ when String.ends_with ~suffix:"map:" line -> (ranges, rules)
  | _ -> (ranges, add_rule line rules)
in

let filename = "src/05/in.txt" in
let lines = Utils.read_lines_from_file filename in
let ranges = get_initial_ranges (List.hd lines) in
let (ranges, rules) = List.fold_left process_line (ranges, []) (List.tl (List.tl lines)) in
let ranges = sort_ranges (map_ranges ranges rules) in
(* print_endline "ranges count:";
(List.length ranges) |> string_of_int |> print_endline;
List.iter (fun r -> (string_of_int r.left) ^ ":" ^ (string_of_int r.right) |> print_endline) ranges; *)
let min = (List.hd ranges).left in
  string_of_int min |> print_endline;
