let read_galaxies lines =
  let rec parse_line i line pos galaxies rows cols =
    match pos = String.length line with
    | true -> (galaxies, rows, cols)
    | false ->
        let c = String.get line pos in
        match c with
        | '#' ->
          rows.(i) <- 1;
          cols.(pos) <- 1;
          let galaxies = (i, pos)::galaxies in
          parse_line i line (pos+1) galaxies rows cols
        | _ -> parse_line i line (pos+1) galaxies rows cols
  in

  let rec do_read i lines galaxies rows cols =
    match lines with
    | [] -> (galaxies, rows, cols)
    | hd::tl ->
      let (galaxies, rows, cols) = parse_line i hd 0 galaxies rows cols in
      do_read (i+1) tl galaxies rows cols
  in

  let rows = Array.init (List.length lines) (fun _ -> 0) in
  let cols = Array.init (List.length lines) (fun _ -> 0) in
  do_read 0 lines [] rows cols
in

let spaces_in_interval arr a b =
  Myarray.fold_lefti (
    fun acc k v ->
      match v = 0 && k > (min a b) && k < (max a b) with
      | true -> acc+1
      | false -> acc
  ) 0 arr
in

let distance_from_galaxy_to v list rows cols =
  let rec do_get (x, y) list rows cols acc =
    match list with
    | [] -> acc
    | (a, b)::tl ->
      abs (x - a) +
      (spaces_in_interval rows x a) +
      abs (y - b) +
      (spaces_in_interval cols y b) +
      do_get (x, y) tl rows cols acc
  in

  do_get v list rows cols 0
in

let rec sum_distances list rows cols =
  match list with
  | _ :: [] -> 0
  | hd::tl -> distance_from_galaxy_to hd tl rows cols + sum_distances tl rows cols
  | _ -> 0
in

let filename = "src/11/in.txt" in
let lines = Utils.read_lines_from_file filename in
let (galaxies, rows, cols) = read_galaxies lines in
let res = sum_distances galaxies rows cols in
  string_of_int res |> print_endline
