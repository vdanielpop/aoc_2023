let read_lines_from_file filename = 
  let rec read_and_append arr fin  = 
    try
      let line = input_line fin in
      read_and_append (line :: arr) fin
    with
    | End_of_file -> arr
  in

  let fin = open_in filename in
  let lines = read_and_append [] fin in
  close_in fin;
  List.rev(lines)
;;

let is_digit = function '0' .. '9' -> true | _ -> false
