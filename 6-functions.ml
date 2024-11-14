(** Generate a random list. *)
let random_seq n min max =
  List.init n (fun _ -> min + Random.int (max - min + 1))

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
  
(** Merge sort with definable comparator function. *)
let rec merge cmp x y =
  match x, y with
    [], l | l, [] -> l
  | hx::tx, hy::ty ->
    if cmp hx hy
    then hx :: merge cmp tx y
    else hy :: merge cmp x ty

let rec msort cmp l =
  match l with
    [] -> []
  | [x] -> [x]
  | _ ->
    let n = List.length l / 2 in
    let left = take n l in
    let right = drop n l in
    merge cmp (msort cmp left) (msort cmp right)

(** Q! Recursive function to replace all exclamation marks in a char list
    with periods. *)
let rec exclamation_to_period l : char list =
  match l with
    [] -> []
  | x :: xs ->
    if x = '!'
    then '.' :: exclamation_to_period xs
    else x :: exclamation_to_period xs

(** Q2: Higher-order function funtcion to replace all exclamation marks in char list
    with periods. *)
let excl_to_period l : char list =
  List.map (fun c -> if c = '!' then '.' else c) l

(** Q3: Clip a number to be between 1 and 10 inclusive. *)
let clip n =
  if n < 1 then 1
  else if n > 10 then 10
  else n

(** Clip all numbers in a list. *)
let clip_list l =
  List.map clip l

let clip_list' l =
  List.map (fun n -> if n < 1 then 1 else if n > 10 then 10 else n) l

(** Q4: Apply a function the given number of times to an initial value. *)
let rec apply f n x =
  if n = 0 then x else f (apply f (n - 1) x)

(** Q5: Insertion sort with user-definable comparison function. *)
(** Insertion sort in one function. *)
let rec isort cmp l =
  let rec insert cmp x l =
    match l with
      [] -> [x]
    | h :: t -> if cmp x h then x :: h :: t else h :: insert cmp x t in
  match l with
    [] -> []
  | h::t -> insert cmp h (isort cmp t)

(** Q6: Write a filter function that takes a predicate and a list and returns
    those list items where the predicate is true. *)
let rec filter f l =
  match l with
    [] -> []
  | x :: xs -> if f(x) then x :: filter f xs else filter f xs

(** Q7: Return true if the predicate is true for all elements in the list or
    false otherwise. *)
let rec for_all pred l =
  match l with
    [] -> true
  | x :: xs -> pred x && for_all pred xs

(** Q8: Implement mapl for mapping over lists of lists. *)
let rec mapl (f: 'a -> 'b) (l: 'a list list) : 'b list list =
  match l with
    [] -> []
  | l :: ls -> List.map f l :: mapl f ls
