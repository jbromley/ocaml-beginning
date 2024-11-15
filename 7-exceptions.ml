(** Generate a list of integers from 1 to n, inclusive. *)
let rec seq n =
  let rec seq' n l =
    match n with
      0 -> l
    | _ -> seq' (n - 1) (n :: l) in
  seq' n []

(** Generate a random list. *)
let random_seq n min max =
  List.init n (fun _ -> min + Random.int (max - min + 1))
    
(** Take n elements from the front of a list. *)
let rec take n l =
  match l with
    [] -> if n = 0 then [] else raise (Invalid_argument "take")
  | x :: xs ->
    if n < 0 then raise (Invalid_argument "take") else
    if n = 0 then [] else x :: take (n - 1) xs

(** Drop n elements from the front of a list. *)
let rec drop n l =
  match l with
    [] -> if n = 0 then [] else raise (Invalid_argument "drop")
  | x :: xs ->
    if n < 0 then raise (Invalid_argument "drop") else
    if n = 0 then l else drop (n - 1) xs
        
(** Return the last element of a list. *)
let rec last l =
  match l with
    [] -> raise Not_found
  | [x] -> x
  | _::xs -> last xs

(** Q1: Return the smallest positive element of a list of integers. Raise a
    Not_found exception if there is no positive integer in the list. *)
let smallest (l: int list) =
  let rec smallest' current found l =
    match l with
      [] -> if found then current else raise Not_found
    | x :: xs ->
      if x > 0 && x <= current
      then smallest' x true xs
      else smallest' current found xs in
  smallest' max_int false l

(** Q2: Return the smallest positive element of a list of integers. Return
    zero if there is no positive integer in the list. *)
let smallest_or_zero l =
  try smallest l with
    Not_found -> 0

exception Complex
  
(** Q3: Calculate the largest integer smaller than the square root of a
    given number. Raise a Complex exception if the number is negative. *)
let isqrt n =
  let rec isqrt' x n =
    if x * x > n then (x - 1) else isqrt' (x + 1) n in
  if n < 0 then raise Complex else isqrt' 1 n

(** Q3: Calculate the largest integer smaller than the square root of a
    given number. Return zero if the number is negative. *)
let isqrt_safe n =
  try isqrt n with Complex -> 0

