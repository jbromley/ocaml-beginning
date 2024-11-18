(** Find the length of a list. *)
let rec length l =
  match l with
    [] -> 0
  | h :: t -> 1 + length t

(** Take the first n elements of a list. *)
let rec take n l =
  if n = 0 then [] else
    match l with
      [] -> []
    | h :: t -> h :: take (n - 1) t

(* Map a function over a list. *)
let rec map f l =
  match l with
    [] -> []
  | h :: t -> f h :: map f t
                
(* Determine if a value is a member of a list. *)
let rec member x l =
  match l with
    [] -> false
  | h :: t -> if x = h then true else member x t

(** 2. Determine if a value is a member of all lists. *)
let member_all x ls =
  let contains = map (member x) ls in
  not (member false contains)

(** 3. Divide the second number by the first number. *)
let rdiv x y = y / x

(** 4. Map a function over a list of lists of lists. *)
let mapll f l = map (map (map f)) l

(** Truncate a list to n elements. *)
let truncate_l n l =
  if length l >= n then take n l else l
    
(** 5. Take an integer and a list of lists and return a list where each of the
    internal lists have been truncated to the number of elements. *)
let truncate n ll =
  map (truncate_l n) ll

(** Return the first element of a list. *)
let firstelt n l =
  match l with
    [] -> n
  | h :: _ -> h

(** 6. Take a list of list of integers and return the first element of each
    list. If a list is empty return the default value. *)
let firstelts n ll =
  map (firstelt n) ll

