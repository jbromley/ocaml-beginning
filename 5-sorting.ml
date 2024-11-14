open List
    
(* Chapter 6: Sorting Things *)

(** Generate a list of integers from 1 to n, inclusive. *)
let rec seq n =
  let rec seq' n l =
    match n with
      0 -> l
    | _ -> seq' (n - 1) (n :: l) in
  seq' n []

(** Generate a random list. *)
let random_seq n max =
  List.init n (fun _ -> Random.int max)
    
(** Take the first n elements of a list *)
let rec take n l =
  match (n, l) with
  | (0, _) -> []
  | (_, []) -> []
  | (_, x :: xs) -> x :: take (n - 1) xs

(** Drop the first n elements of a list. *)
let rec drop n l =
  match (n, l) with
  | (0, _) -> l
  | (_, []) -> []
  | (_, _ :: xs) -> drop (n - 1) xs
  
let rec insert x l =
  match l with
    [] -> [x]
  | h::t when x <= h -> x :: h :: t
  | h::t -> h :: insert x t

(** Sort a list using insertion sort, *)
let rec isort l =
  match l with
    [] -> []
  | h::t -> insert h (isort t)

let rec merge x y =
  match x, y with
    [], l -> l
  | l, [] -> l
  | hx::tx, hy::ty ->
    if hx < hy
    then hx :: merge tx y
    else hy :: merge x ty

(* (\** Sort a list using merge sort. *\) *)
let rec msort l =
  match l with
    [] -> []
  | [x] -> [x]
  | _ ->
    let n = List.length l / 2 in
    let left = take n l in
    let right = drop n l in
    merge (msort left) (msort right)

(* Insertion sort that puts a list in reverse order. *)
let rec insert_r x l =
  match l with
    [] -> [x]
  | h::t ->  if x >=h then x :: h :: t else h :: insert_r x t

let rec risort l =
  match l with
    [] -> []
  | h::t -> insert_r h (risort t)

(** Detect if a list is already sorted. *)
let is_sorted l =
  let rec is_sorted' l last =
    match l with
      [] -> true
    | x :: xs -> if x < last then false else is_sorted' xs x in
  is_sorted' (List.tl l) (List.hd l)

(** Insertion sort in one function. *)
let rec insertion_sort l =
  let rec insert x l =
    match l with
      [] -> [x]
    | h::t -> if x <=h then x :: h :: t else h :: insert x t in
  match l with
    [] -> []
  | h::t -> insert h (insertion_sort t)
