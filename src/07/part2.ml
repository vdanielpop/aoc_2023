let card_values = Hashtbl.create 13 in
Hashtbl.add card_values 'J' 1;
Hashtbl.add card_values '2' 2;
Hashtbl.add card_values '3' 3;
Hashtbl.add card_values '4' 4;
Hashtbl.add card_values '5' 5;
Hashtbl.add card_values '6' 6;
Hashtbl.add card_values '7' 7;
Hashtbl.add card_values '8' 8;
Hashtbl.add card_values '9' 9;
Hashtbl.add card_values 'T' 10;
Hashtbl.add card_values 'Q' 11;
Hashtbl.add card_values 'K' 12;
Hashtbl.add card_values 'A' 13;

let hand_values = Hashtbl.create 16 in
Hashtbl.add hand_values "11111" 1;
Hashtbl.add hand_values "1112" 2;
Hashtbl.add hand_values "1121" 2;
Hashtbl.add hand_values "1211" 2;
Hashtbl.add hand_values "2111" 2;
Hashtbl.add hand_values "122" 3;
Hashtbl.add hand_values "212" 3;
Hashtbl.add hand_values "221" 3;
Hashtbl.add hand_values "311" 4;
Hashtbl.add hand_values "131" 4;
Hashtbl.add hand_values "113" 4;
Hashtbl.add hand_values "32" 5;
Hashtbl.add hand_values "23" 5;
Hashtbl.add hand_values "41" 6;
Hashtbl.add hand_values "14" 6;
Hashtbl.add hand_values "5" 7;

let value_with_joker hand_type j = 
  match hand_type, j with
  | x, 0 -> Hashtbl.find hand_values x
  | _, 5 -> 7
  | "1112", _ | "1121", _ | "1211", _ | "2111", _ -> 4
  | "122", 1 | "212", 1 | "221", 1 -> 5
  | "122", 2 | "212", 2 | "221", 2 -> 6
  | "311", _ | "131", _ | "113", _ -> 6
  | "32", _ | "23", _-> 7
  | "14", _ | "41", _-> 7
  | _ -> Hashtbl.find hand_values hand_type + j
in

let add_or_inc h k =
  if Hashtbl.mem h k then
    Hashtbl.replace h k (Hashtbl.find h k + 1)
  else
    Hashtbl.add h k 1;
in

let extract_triplet line =

  let hand_type str =
    let h = Hashtbl.create 5 in
    String.iter (add_or_inc h) str;
    let js = match Hashtbl.find_opt h 'J' with
    | Some x -> x
    | None -> 0
    in
    let list = List.of_seq (Hashtbl.to_seq_values h) in
    (List.fold_left (fun x y -> x ^ string_of_int y) "" list, js)
  in

  let parts = String.split_on_char ' ' line in
  let (hand_type, js) = hand_type (List.hd parts) in
  (List.hd parts, value_with_joker hand_type js, List.tl parts |> List.hd |> int_of_string)
in

let hand_cmp (a, atv, _av) (b, btv, _bv) =
  let rec card_cmp a b i =
    match i with
    | 5 -> 0
    | _ ->
      let ac = Hashtbl.find card_values a.[i] in
      let bc = Hashtbl.find card_values b.[i] in
      if ac = bc then
        card_cmp a b (i+1)
      else
        ac - bc
  in

  if atv < btv then
    -1
  else if atv > btv then
    1
  else
    card_cmp a b 0
in

let filename = "src/07/in.txt" in
let lines = Utils.read_lines_from_file filename in
let pairs = List.map extract_triplet lines in
let sorted = List.sort hand_cmp pairs in
let res = List.mapi (fun i (_a, _at, b)-> (i+1) * b) sorted |> List.fold_left (fun x y -> x + y) 0 in
  string_of_int res|> print_endline
