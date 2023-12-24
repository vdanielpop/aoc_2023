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

  let get_by_instruction v h i = 
    let (a, b) = Hashtbl.find h v in
    if i = 'R' then
      b
    else
      a
  in

  let rec get_steps_until_loop h l c v = 
    match String.ends_with ~suffix:"Z" v with
    | true -> c
    | _ -> 
      let new_v = get_by_instruction v h (List.hd l) in
      get_steps_until_loop h (next l) (c+1) new_v
  in

  let initial_values = Hashtbl.to_seq_keys nodes_tbl |> Seq.filter (fun x -> String.ends_with x ~suffix:"A") in
  let loops = Seq.map (get_steps_until_loop nodes_tbl instructions 0) initial_values in
  Seq.fold_left (fun a b -> Utils.lcm a b) 1 loops
in

let filename = "src/08/in.txt" in
let lines = Utils.read_lines_from_file filename in
let instructions = List.of_seq (String.to_seq (List.hd lines)) in
let nodes_tbl = Hashtbl.create (List.length lines) in
List.tl (List .tl lines) |> extract_nodes nodes_tbl;
let res = compute_steps instructions nodes_tbl in
  string_of_int res|> print_endline;
print_newline();
