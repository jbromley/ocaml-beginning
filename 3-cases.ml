(** Perform a logical not on the argument. *)
let not x =
  match x with
  | false -> true
  | true -> false

(** Sum the number from 1 to n. *)
let rec sum n =
  match n with
  | 0 -> 0
  | n -> n + sum (n - 1)

(** Raise the floating point number x to the integral power n. *)
let rec power x n =
  match n with
  | 0 -> 1.0
  | n when n > 0 -> x *. power x (n - 1)
  | n -> (1.0 /. x) *. power x (n + 1)

(** Return true is a character is upper case. *)
let is_upper c =
  match c with
  | 'A'..'Z' -> true
  | _ -> false
    
(** Return true is a character is lower case. *)
let is_lower c =
  match c with
  | 'a'..'z' -> true
  | _ -> false
    
