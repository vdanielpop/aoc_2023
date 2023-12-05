let calibration_value_from_line line =
  let rec get_first_digit str = 
    match str with
    | "" -> ""
    | _ -> 
      if Utils.is_digit(str.[0]) 
      then 
        String.make 1 str.[0] 
      else
        get_first_digit(String.sub str 1 (String.length(str) - 1))
  in

  let intval str =
    match str with
    | "" -> 0
    | _ -> int_of_string(str)
  in

  intval (get_first_digit line ^ get_first_digit (Mystring.reverse line))
in

let filename = "src/01/in.txt" in
let lines = Utils.read_lines_from_file filename in
let numbers = List.map calibration_value_from_line lines in
let sum = List.fold_left (fun x y -> x + y) 0 numbers in
  print_int sum |> print_newline
