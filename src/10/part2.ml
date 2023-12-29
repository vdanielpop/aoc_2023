module Pair =
  struct
    type t = int * int
    let compare (x1, y1) (x2, y2) =
      match compare x1 x2 with
      | 0 -> compare y1 y2
      | c -> c
  end

module PairMap = Map.Make(Pair)
let loop_points = PairMap.empty;;


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

let west_loop arr x y =
  match y > 0 with
  | false -> false
  | true -> Mylist.contains ['-';'L';'F'] (String.get (arr.(x)) (y-1))
in

let north_loop arr x y =
  match x > 0 with
  | false -> false
  | true -> Mylist.contains ['|';'7';'F'] (String.get (arr.(x-1)) (y))
in

let east_loop arr x y =
  match y < String.length (arr.(0)) with
  | false -> false
  | true -> Mylist.contains ['-';'J';'7'] (String.get (arr.(x)) (y+1))
in


let south_loop arr x y =
  match x < Array.length arr - 1 with
  | false -> false
  | true -> Mylist.contains ['|';'L';'J'] (String.get (arr.(x+1)) (y))
in

let walk_loop map arr xs ys =
  let rec do_walk map x y dir =
    let current = String.get (arr.(x)) y in
    let map = PairMap.add (x, y) current map in
    match current with
    | 'S' -> map
    | _ ->
      let dest_dir = pipe_from current dir in
      let (xd, yd) = pos_from_direction x y dest_dir in
      do_walk map xd yd (opposite dest_dir)
  in

  if west_loop arr xs ys then
    do_walk map xs (ys-1) 'E'
  else if north_loop arr xs ys then
    do_walk map (xs-1) ys 'S'
  else if east_loop arr xs ys then
    do_walk map xs (ys+1) 'W'
  else
    do_walk map (xs+1) ys 'N'
in

let get_start_replacement arr x y =
  match true with
  | _ when west_loop arr x y && north_loop arr x y -> 'J'
  | _ when west_loop arr x y && east_loop arr x y -> '-'
  | _ when west_loop arr x y && south_loop arr x y -> '7'
  | _ when north_loop arr x y && east_loop arr x y -> 'L'
  | _ when north_loop arr x y && south_loop arr x y -> '|'
  | _ when east_loop arr x y && south_loop arr x y -> 'F'
  | _ ->
    string_of_int x |> print_endline;
    string_of_int y |> print_endline;
    failwith "Should always find 2 ways to connect from start."
in

let count_enclosed arr map =
  let next (x, y) stack =
    let max = String.length (arr.(x)) in
    match y = max - 1 with
    | true -> (x+1, 0, [])
    | false -> (x, y+1, stack)
  in

  let inside stack =
    match stack with
    | [] -> false
    | hd::_ -> hd = '|' && List.length stack mod 2 = 1
  in

  let push stack c =
    match stack with
    | [] -> [c]
    | hd::tl ->
      match hd, c with
      |'|','|' -> tl
      |'|','F' -> 'F'::'|'::tl
      |'|','L' -> 'L'::'|'::tl
      |'F','-' -> 'F'::tl
      |'F','J' -> '|'::tl
      |'F','7' -> tl
      |'L','-' -> 'L'::tl
      |'L','J' -> tl
      |'7','F' -> tl
      |'L','7' -> '|'::tl
      | _ -> failwith ("Push not handled for " ^ (String.make 1 hd) ^ (String.make 1 c))
  in

  let rec do_count (x, y) count stack =
    match x = Array.length arr with
    | true -> count
    | false ->
      match PairMap.find_opt (x, y) map with
      | None ->
        let (nx, ny, nstack) = next (x, y) stack in
        if inside stack then
          do_count (nx, ny) (count+1) nstack
        else
          do_count (nx, ny) count nstack
      | Some c ->
        let nstack = push stack c in
        let (nx, ny, nstack) = next (x, y) nstack in
        do_count (nx, ny) count nstack
  in

  do_count (0, 0) 0 []
in


let filename = "src/10/in.txt" in
let lines = Utils.read_lines_from_file filename in
let field = Array.init (List.length lines) (Mylist.get lines) in
let (xs, ys) = find_start field in

let loop_points = walk_loop loop_points field xs ys in
let c = get_start_replacement field xs ys in
field.(xs) <- Mystring.set (field.(xs)) ys c;
let loop_points = PairMap.add (xs, ys) c loop_points in
let res = count_enclosed field loop_points in
string_of_int res |> print_endline
