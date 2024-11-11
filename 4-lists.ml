(** Return all even numbers in a list. *)
let evens l =
  let rec evens_aux l acc =
    match l with
    | [] -> acc
    | h::t when h mod 2 = 0 -> evens_aux t (h::acc)
    | h::t -> evens_aux t acc in
  evens_aux l []

(** Count the number of true elements in a list of booleans. *)
let rec count_true l =
  match l with
  | [] -> 0
  | h::t when h = true -> 1 + count_true t
  | h::t -> count_true t

(** Count the number of true elements in a list of booleans. Ensure
    the function is tail-recursive. *)
let rec count_true_r l =
  let rec count_true_aux l acc =
    match l with
    | [] -> acc
    | h::t when h = true -> count_true_aux t (acc + 1)
    | h::t -> count_true_aux t acc in
  count_true_aux l 0

(** Make a list into a palindrome. *)
let make_palindrome l =
  l @ List.rev l

(** Determine if a list is a palindrome. *)
let is_palindrome l =
  l = List.rev l

(** Return all but the last element of a list. *)
let rec drop_last l =
  match l with
  | [] | _::[] -> []
  | h::t -> [h] @ drop_last t

(** Return all but the last element of a list. *)
let drop_last_r l =
  let rec drop_last_aux l acc =
    match l with
    | [] | _::[] -> List.rev acc
    | h::t -> drop_last_aux t (h::acc) in
  drop_last_aux l []

(** Determine if a given value is in a list. *)
let rec member elem l =
  match l with 
  | [] -> false
  | h::t when h = elem -> true
  | h::t -> member elem t

(** Make a set out of a list. *)
let make_set l =
  let rec make_set_aux l acc =
    match l with
    | [] -> List.rev acc
    | h::t when member h acc -> make_set_aux t acc
    | h::t -> make_set_aux t (h::acc) in
  make_set_aux l []
