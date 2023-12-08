let calibration_value_from_line line =
  let get_spelled_digit str =
    match true with
    | _ when (String.starts_with ~prefix:"one" str) -> "1"
    | _ when (String.starts_with ~prefix:"two" str)-> "2"
    | _ when (String.starts_with ~prefix:"three" str) -> "3"
    | _ when (String.starts_with ~prefix:"four" str) -> "4"
    | _ when (String.starts_with ~prefix:"five" str) -> "5"
    | _ when (String.starts_with ~prefix:"six" str) -> "6"
    | _ when (String.starts_with ~prefix:"seven" str) -> "7"
    | _ when (String.starts_with ~prefix:"eight" str) -> "8"
    | _ when (String.starts_with ~prefix:"nine" str) -> "9"
    | _ -> str
  in
  let get_rev_spelled_digit str =
    match true with
    | _ when (String.starts_with ~prefix:"eno" str) -> "1"
    | _ when (String.starts_with ~prefix:"owt" str)-> "2"
    | _ when (String.starts_with ~prefix:"eerht" str) -> "3"
    | _ when (String.starts_with ~prefix:"ruof" str) -> "4"
    | _ when (String.starts_with ~prefix:"evif" str) -> "5"
    | _ when (String.starts_with ~prefix:"xis" str) -> "6"
    | _ when (String.starts_with ~prefix:"neves" str) -> "7"
    | _ when (String.starts_with ~prefix:"thgie" str) -> "8"
    | _ when (String.starts_with ~prefix:"enin" str) -> "9"
    | _ -> str
  in

  let rec get_first_digit str f =
    match str with
    | "" -> ""
    | _ ->
      if Utils.is_digit(str.[0])
      then
        String.make 1 str.[0]
      else
        let spelled_digit = f str in
        if Utils.is_digit(spelled_digit.[0])
        then
          spelled_digit
        else
          get_first_digit(String.sub str 1 (String.length(str) - 1)) f
  in

  let intval str =
    match str with
    | "" -> 0
    | _ -> int_of_string(str)
  in

  intval (get_first_digit line get_spelled_digit ^ get_first_digit (Mystring.reverse line) get_rev_spelled_digit)
in

let filename = "src/01/in.txt" in
let lines = Utils.read_lines_from_file filename in
let numbers = List.map calibration_value_from_line lines in
let sum = List.fold_left (fun x y -> x + y) 0 numbers in
  print_int sum |> print_newline
