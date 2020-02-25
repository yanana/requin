let splitString s =
  let rec split i acc = if i >= 0 then split (i - 1) (s.[i] :: acc) else acc in
  split (String.length s - 1) []

let padStart n c s =
  let l = String.length s in
  if l >= n then s else String.make (n - l) c ^ s

let explode s =
  let rec explode' i l = if i < 0 then l else explode' (i - 1) (s.[i] :: l) in
  explode' (String.length s - 1) []

open Belt

let moveToFirst i xs =
  xs |. List.get i
  |. Option.flatMap (fun x ->
         xs |. List.take i
         |. Option.flatMap (fun init ->
                xs
                |. List.drop (i + 1)
                |. Option.map (fun tail -> (x :: init) @ tail)))
  |. Option.getWithDefault xs

let findIndex p xs =
  let rec f i xs =
    match xs with
    | [] -> None
    | [ a ] -> if p a then Some i else None
    | a :: rest -> if p a then Some i else f (i + 1) rest
  in
  f 0 xs
