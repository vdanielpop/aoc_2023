let rec extract_nodes h lines =
  match lines with
  | [] -> ()
  | hd :: tl ->(
    let k = String.sub hd 0 3 in
    let v = (String.sub hd 7 3, String.sub hd 12 3) in
    Hashtbl.add h k v;
    extract_nodes h tl
  )
in

let compute_steps instructions nodes_tbl =
  let next l =
    match l with
    | _::[] -> instructions
    | _::tl -> tl
    | [] -> raise (Invalid_argument "Wrong list of instructions")
  in

  let rec do_step h k l c =
    let ins = List.hd l in
    match Hashtbl.find h k, ins with
    | (_, x), 'R' -> if x = "ZZZ" then c else do_step h x (next l) (c+1)
    | (x, _), 'L' -> if x = "ZZZ" then c else do_step h x (next l) (c+1)
    | _ -> raise (Invalid_argument "Incorrect construction of the Hashtbl.")
  in

  do_step nodes_tbl "AAA" instructions 1
in
let filename = "src/08/in.txt" in
let lines = Utils.read_lines_from_file filename in
let instructions = List.of_seq (String.to_seq (List.hd lines)) in
let nodes_tbl = Hashtbl.create (List.length lines) in
List.tl (List .tl lines) |> extract_nodes nodes_tbl;
let res = compute_steps instructions nodes_tbl in
  string_of_int res|> print_endline
