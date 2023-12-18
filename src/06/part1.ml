let extract_values line =
  let parts = String.split_on_char ':' line in
  let string_vals = Mystring.trim_split_on_char ' ' (List.hd (List.tl parts)) in
  List.map int_of_string string_vals
in

let compute_way (t,d) =
  let delta = t*t - 4*d |> float_of_int |> Float.sqrt in

  match delta *. delta = float_of_int ((int_of_float delta) * (int_of_float delta)) with
  | false -> int_of_float (((float_of_int t) +. delta) /. 2.) - int_of_float (((float_of_int t) -. delta) /. 2.)
  | true -> int_of_float (((float_of_int t) +. delta) /. 2.) - int_of_float (((float_of_int t) -. delta) /. 2.) - 1

in


let filename = "src/06/in.txt" in
let lines = Utils.read_lines_from_file filename in
let times = extract_values (List.hd lines) in
let distances = extract_values (List.hd (List.tl lines)) in
let pairs = List.combine times distances in
let ways_per_race = List.map compute_way pairs in
let res = List.fold_left (fun x y -> x * y) 1 ways_per_race in
  string_of_int res |> print_endline;
