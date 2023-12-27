let pipe_matches = Hashtbl.create 20;;
Hashtbl.add


let find_start arr = 
  let rec do_find i = 
    match Mystring.find_opt (arr.(i)) 'S' with
    | None -> do_find (i+1)
    | Some x -> (i, x)
  in

  do_find 0
in


let walk_loop arr (xs, ys) = 
  let rec do_walk x y c = 
    

let filename = "src/10/in.txt" in
let lines = Utils.read_lines_from_file filename in
let field = Array.init (List.length lines) (Mylist.get lines) in
let (xs, ys) = find_start field in
let lists = List.map (fun x -> String.split_on_char ' ' x |> List.rev |> List.map int_of_string) lines in
let predictions = List.map get_prediction lists in
let res = List.fold_left (fun x y -> x+y) 0 predictions in
  string_of_int res |> print_endline 