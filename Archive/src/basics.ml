let abs x =
  if x >= 0 then x
  else -x

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup =
  let a = fst tup in
  let b = snd tup in
  (b, a)

let rev_triple tup =
  let (a, b, c) = tup in
  (c, b, a)

let is_odd x =
  if x mod 2 = 0 then false
  else true

let is_older date1 date2 =
  let (y1, m1, d1) = date1 in
  let (y2, m2, d2) = date2 in
  if y1 < y2 then true
  else if y1 > y2 then false
  else if m1 < m2 then true
  else if m1 > m2 then false
  else if d1 < d2 then true
  else false

let to_us_format date1 =
  let (y, m, d) = date1 in
  (m, d, y)

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec pow x p =
  if p = 0 then 1
  else x * pow x (p - 1)

let rec fac n =
  if n = 0 then 1
  else n * fac (n - 1)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth (idx, lst) =
  match lst with
  | [] -> failwith "index out of bounds"
  | h :: t ->
      if idx = 0 then h
      else get_nth (idx - 1, t)

let rec length lst =
  match lst with
  | [] -> 0
  | _ :: t -> 1 + length t

let larger lst1 lst2 =
  let l1 = length lst1 in
  let l2 = length lst2 in
  if l1 > l2 then lst1
  else if l2 > l1 then lst2
  else []

let rec sum_list lst =
  match lst with
  | [] -> 0
  | h :: t -> h + sum_list t

let sum lst1 lst2 =
  sum_list lst1 + sum_list lst2
