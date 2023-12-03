let read_lines_from_file filename = 
  let rec read_and_append arr fin  = 
    try
      let line = input_line fin in
      read_and_append (arr @ [line]) fin
    with
    | End_of_file -> arr
  in

  let fin = open_in filename in
  let lines = read_and_append [] fin in
  close_in fin;
  lines
;;

let calibration_value_from_line line =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let concat_if_digit = fun s c -> if is_digit(c) then s ^ String.make 1 c else s in
  let explode s = List.init (String.length s) (String.get s) in
  let rec implode l = 
    match l with
    | [] -> ""
    | [a] -> String.make 1 a
    | hd :: rest -> String.make 1 hd ^ implode(rest)
  in 
  
  let numbers_in_line = String.fold_left concat_if_digit "" line in
  let numbers_list = explode(numbers_in_line) in
  let numbers_tuple = [List.hd(numbers_list); List.hd(List.rev(numbers_list))] in
  implode numbers_tuple |> int_of_string
;;

let filename = "in.txt" in
let lines = read_lines_from_file filename in
let numbers = List.map calibration_value_from_line lines in
let sum = List.fold_left (fun x y -> x + y) 0 numbers in
  print_int sum;
