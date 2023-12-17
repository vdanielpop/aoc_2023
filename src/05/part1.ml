type rule = {src_start: int; src_end:int; dest_start: int;};;

let get_seeds line =
  let parts = String.split_on_char ':' line in
  let string_vals = Mystring.trim_split_on_char ' ' (List.hd (List.tl parts)) in
  List.map int_of_string string_vals
in

let process_mappings mappings seeds  =
  let rec do_process mappings seed =
    match mappings with
    | [] -> seed
    | {src_start; src_end; dest_start;}::tl ->
      if src_start <= seed && seed <= src_end
      then
        dest_start + (seed - src_start)
      else
        do_process tl seed
  in
  List.map (do_process mappings) seeds
in

let add_mapping line mappings =
  let parts = Mystring.trim_split_on_char ' ' line in
  let int_parts = List.map int_of_string parts in
  match int_parts with
  | dest::src::len::_ -> ({src_start = src; src_end = src + len - 1; dest_start = dest;}::mappings)
  | _ -> raise (Invalid_argument "mapping")
in

let process_line (seeds, mappings) line =
  match true with
  | _ when String.length line = 0 -> ((process_mappings mappings seeds), [])
  | _ when String.ends_with ~suffix:"map:" line -> (seeds, mappings)
  | _ -> (seeds, add_mapping line mappings)
in

let filename = "src/05/in.txt" in
let lines = Utils.read_lines_from_file filename in
let seeds = get_seeds (List.hd lines) in
let (pre_values, mappings) = List.fold_left process_line (seeds, []) (List.tl (List.tl lines)) in
let values = process_mappings mappings pre_values in
let min = List.fold_left (fun x y -> if x <= y then x else y) (List.hd values) values in
  (* List.iter (fun x -> print_endline (string_of_int x)) values; *)
  string_of_int min |> print_endline;
