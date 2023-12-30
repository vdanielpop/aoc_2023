let fold_lefti f init arr =
  let rec aux acc i =
    if i < Array.length arr then
      aux (f acc i arr.(i)) (i + 1)
    else
      acc
  in
  aux init 0
