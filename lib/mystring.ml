let rev_get str i = String.get str (String.length str - 1 - i)

let reverse str = String.init (String.length str) (rev_get str)

let explode str = List.init (String.length str) (String.get str)

let trim_split_on_char sep s =
  let rec do_split s pos list buffer =
    match true with
    | _ when pos = -1 && buffer != "" -> (buffer :: list)
    | _ when pos = -1 -> list
    | _ when s.[pos] = sep && buffer != "" -> do_split s (pos - 1) (buffer :: list) ""
    | _ when s.[pos] = sep -> do_split s (pos - 1) list ""
    | _ -> do_split s (pos - 1) list (String.make 1 s.[pos] ^ buffer)
  in

  let len = String.length s in
  do_split s (len - 1) [] ""

let find_opt s c =
  let rec do_find c i =
    match true with
    | _ when i >= String.length s -> None
    | _ when String.get s i = c -> Some i
    | _ -> do_find c (i+1)
  in

  do_find c 0
