let max_red = 12 in
let max_green = 13 in
let max_blue = 14 in

let get_valid_game_id line =
  let get_cube_amount s =
    String.trim s |> String.split_on_char ' ' |> List.hd |> int_of_string
  in

  let process_cube_draw (r, g, b) s =
    match true with
    | _ when String.ends_with ~suffix:"red" s -> (Stdlib.max r (get_cube_amount s), g, b)
    | _ when String.ends_with ~suffix:"green" s -> (r, Stdlib.max g (get_cube_amount s), b)
    | _ when String.ends_with ~suffix:"blue" s -> (r, g, Stdlib.max b (get_cube_amount s))
    | _ -> (r, g, b)
  in

  let process_turn draws tuple =
    List.fold_left process_cube_draw tuple (String.split_on_char ',' draws)
  in

  let max_tuple (a, b, c) (d, e, f) = (Stdlib.max a d, Stdlib.max b e, Stdlib.max c f) in

  let rec process_turns turns tuple =
    match turns with
    | [] -> tuple
    | hd :: tl -> max_tuple (process_turn hd tuple) (process_turns tl tuple)
  in

  let line_parts = String.split_on_char ':' line in
  let gid = String.split_on_char ' ' (List.hd line_parts) |> List.tl |> List.hd in
  let turns = String.split_on_char ';' (List.hd (List.tl line_parts)) in
  let (r, g, b) = process_turns turns (0, 0, 0) in
  match true with
  | _ when r <= max_red && g <= max_green && b <= max_blue -> int_of_string gid
  | _ -> 0
in

let filename = "src/02/in.txt" in
let lines = Utils.read_lines_from_file filename in
let game_ids = List.map get_valid_game_id lines in
let sum = List.fold_left (fun x y -> x + y) 0 game_ids in
  print_int sum |> print_newline
