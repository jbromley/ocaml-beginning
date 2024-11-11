(** Multiply a number by 10. *)
let mult_by_10 n =
  n * 10

(** Return true if both arguments are non-zero and false otherwise. *)
let are_nonzero a b =
  (a <> 0) && (b <> 0)

(** Sum the number from 1 to n. *)
let rec sum n =
  if n = 0 then 0 else n + sum (n - 1)

(** Raise the number x to the nth power. *)
let rec power x n =
  if n = 0 then 1 else x * power x (n - 1)

(** Return true if character is a lower-case consonant and false otherwise. *)
let is_consonant c =
  if c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u' then false else true

