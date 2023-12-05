let rev_get str i = String.get str (String.length str - 1 - i)

let reverse str = String.init (String.length str) (rev_get str)

let explode str = List.init (String.length str) (String.get str)
