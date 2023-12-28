let pipe_from c dir =
  let (a,b) = match c with
    | '|' -> ('N', 'S')
    | '-' -> ('E', 'W')
    | 'L' -> ('N', 'E')
    | 'J' -> ('N', 'W')
    | 'F' -> ('S', 'E')
    | '7' -> ('S', 'W')
    | _ -> failwith "Unknown input character"
  in
  if dir = a then b else a
in

let pos_from_direction x y dir =
  match dir with
  | 'N' -> (x-1, y)
  | 'S' -> (x+1, y)
  | 'E' -> (x, y+1)
  | 'W' -> (x, y-1)
  | _ -> failwith "Unknown direction"
in

let opposite dir =
  match dir with
  | 'N' -> 'S'
  | 'S' -> 'N'
  | 'E' -> 'W'
  | 'W' -> 'E'
  | _ -> failwith "Unknown direction"
in

let find_start arr =
  let rec do_find i =
    match Mystring.find_opt (arr.(i)) 'S' with
    | None -> do_find (i+1)
    | Some x -> (i, x)
  in

  do_find 0
in

let walk_loop arr xs ys =
  let rec do_walk x y dir cnt =
    let current = String.get (arr.(x)) y in
    match current with
    | 'S' -> cnt
    | _ ->
      let dest_dir = pipe_from current dir in
      let (xd, yd) = pos_from_direction x y dest_dir in
      do_walk xd yd (opposite dest_dir) (cnt+1)
  in

  if ys > 0 && Mylist.contains ['-';'L';'F'] (String.get (arr.(xs)) (ys-1)) then
    do_walk xs (ys-1) 'E' 0
  else if xs > 0 && Mylist.contains ['|';'7';'F'] (String.get (arr.(xs-1)) (ys)) then
    do_walk  (xs-1) ys 'S' 0
  else if ys < String.length (arr.(0)) && Mylist.contains ['-';'J';'7'] (String.get (arr.(xs)) (ys+1)) then
    do_walk xs (ys+1) 'W' 0
  else
    do_walk (xs+1) ys 'N' 0
in

let filename = "src/10/in.txt" in
let lines = Utils.read_lines_from_file filename in
let field = Array.init (List.length lines) (Mylist.get lines) in
let (xs, ys) = find_start field in

let steps = walk_loop field xs ys in
let res = Float.ceil (float_of_int steps/.2.) in
  int_of_float res |> string_of_int |> print_endline