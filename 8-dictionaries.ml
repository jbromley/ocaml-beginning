(** Return the first element of a pair. *)
let fst (x, _) = x

(** Return the second element of a pair. *)
let snd (_, y) = y

(** Look up a key's value in a dictionary. *)
let rec lookup key l =
  match l with
    [] -> raise Not_found
  | (k, v) :: xs -> if k = key then v else lookup key xs

(** Add an entry to a dictionary. *)
let rec add k v d =
  match d with
    [] -> [(k, v)]
  | (k', v') :: t -> if k = k' then (k, v) :: t else (k', v') :: add k v t

(** Remove an entry from a dictionary. *)
let rec remove k d =
  match d with
    [] -> []
  | (k', v') :: t -> if k = k' then t else (k', v') :: remove k t

(** Check if a key exists in a dictionary. *)
let key_exists k d =
  try
    let _ = lookup k d in true
  with Not_found -> false

(** Q1: Count the number of different keys in a dictionary *)
let rec count_keys d =
  match d with
    [] -> 0
  | _ :: t -> 1 + count_keys t

(** Q2: Replace the value for a key. If the key does not exist, raise Not_found. *)
let rec replace k v d =
  match d with
    [] -> raise Not_found
  | (k', v') :: t -> if k = k' then (k, v) :: t else (k', v') :: replace k v t

(** Q3: Create a dictionary from a list of keys and a list of values. If the
    lists do not have the same number of elements an Invalid_argument exception
    is raised. *)
let rec mkdict (ks: 'a list) (vs: 'b list) : ('a * 'b) list =
  match ks, vs with
    [], [] -> []
  | [], _ -> raise (Invalid_argument "mkdict: too many values")
  | _, [] -> raise (Invalid_argument "mkdict: too many keys")
  | k :: kt, v :: vt -> (k, v) :: mkdict kt vt

(** Q4: Given a dictionary, return a list of keys and a list of values. *)
let rec mklists d =
  match d with
    [] -> ([], [])
  | (k, v) :: t ->
    let (ks, vs) = mklists t in
    (k :: ks, v :: vs)

(** Q5: Turn a list of pairs into a dictionary. If there are duplicate keys
    then only retain the first occurrence of the key. *)
let pairs_to_dict l =
  let rec pairs_to_dict' l d =
    match l with
      [] -> List.rev d
    | (k, v) :: pt ->
      if key_exists k d then pairs_to_dict' pt d else pairs_to_dict' pt ((k, v) :: d) in
  pairs_to_dict' l []

(** Q6: Given two dictionaries, return the union of those dictionaries. If
    a key is contained in both dictionaries, use the key from the first
    dictionary. *)
let rec union d1 d2 =
  match d1 with
    [] -> d2
  | (k, v) :: t -> add k v (union t d2)
