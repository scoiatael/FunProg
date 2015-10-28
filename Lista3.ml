type polynomial = float list

(* Zadanie 1 *)
let value_at : polynomial -> float -> float =
  let rec aux acc poly x = match poly with
      [] -> acc
    | (p::ps) -> aux (x*.acc +. p) ps x
  in aux 0.

let value_at_list : polynomial -> float -> float = fun poly x ->
  List.fold_left (fun acc p -> x*.acc +. p) 0. poly

(* Zadanie 2 *)
(* value_at_rev p x = value_at (List.rev p) x *)
let value_at_rev : polynomial -> float -> float =
  let rec aux (x_exp, v) poly x = match poly with
      [] -> v
    | (p::ps) -> aux (x*.x_exp, p*.x_exp +. v) ps x
  in aux (1., 0.)

let value_at_rev_list : polynomial -> float -> float = fun poly x ->
  let (_, v) = List.fold_left (fun (x_exp, v) p -> (x*.x_exp, p*.x_exp +. v)) (1.,0.) poly in v

(* Zadanie 3 *)
let diff_rev : polynomial -> polynomial = fun z ->
  let rec aux n b = match b with
    | [] -> []
    | (x::xs) -> (n*.x)::(aux (n +. 1.) xs)
  in aux 1. (List.tl z)

let diff_rev_list : polynomial -> polynomial = fun z ->
  let (_, v) = List.fold_left (fun (n, l) p -> (n +. 1., (p*.n)::l)) (1., []) (List.tl z) in List.rev v


(* Zadanie 4 *)
type column = float list
type row = float list
type matrix = column list

let is_n_n_matrix : matrix -> bool = fun mat ->
  let n = List.length mat in
  List.for_all (fun l -> n = List.length l) mat

let nth_col : matrix -> int -> column = fun mat n -> List.map (fun l -> List.nth l n) mat

let trans : matrix -> matrix = fun mat -> let (_, v) = List.fold_left (fun (n, tra) _ -> (n+1, (nth_col mat n)::tra)) (0, []) mat in List.rev v

let rec zip : 'a list -> 'b list -> ('a * 'b) list = fun xs ys -> match (xs,ys) with
  | ([], []) -> []
  | ([], _) -> failwith "Uneven lists given to zip"
  | (_, []) -> failwith "Uneven lists given to zip"
  | (w::ws, z::zs) -> (w,z)::(zip ws zs)

let zipf : 'a list -> 'b list -> ('a -> 'b -> 'c) -> 'c list = fun xs ys f -> List.map (fun (a,b) -> f a b) (zip xs ys)

let mult_vec : row -> matrix -> column = fun r m ->
  let v1 = zipf r m (fun x v -> List.map (fun a -> a *. x) v) in
  List.map (fun vec -> List.fold_left (fun a b -> a +. b) 0. vec) (trans v1)

let mult_mat : matrix -> matrix -> matrix = fun m1 m2 ->
  List.map (fun v -> mult_vec v m2) m1

(* Zadanie 5 *)

type 'a option = None | Just of 'a

let next_perm : ('a -> 'a -> int) -> 'a list -> 'a list =
  fun gt ->
    let
      sort_desc = List.sort (fun a b -> gt b a)
    in let
      sort_asc = List.sort gt
    in let
      rec pick_lowest_above x ls =
         let
           (above_x, below_x) = List.partition (fun a -> (gt a x) > 0) ls
         in match above_x with
         | [] -> None
         | _ -> let sorted = sort_asc above_x in Just (List.hd sorted, below_x @ List.tl sorted)
    in let
      rec aux set rest = match rest with
        | [] -> sort_desc set
        | (z::zs) -> match pick_lowest_above z set with
          | None -> aux (z::set) zs
          | Just (highest, left) -> (sort_desc (z::left)) @ (highest :: zs)
    in aux []

let all_perms : ('a -> 'a -> int) -> 'a list -> 'a list list =
  fun gt perm ->
    let np = next_perm gt in
    let rec aux cp = if cp = perm
      then [cp]
      else
        cp::(aux (np cp))
    in
    aux (np perm)

(* Zadanie 6 *)

let rec range a b = if a < b then a::(range (a+1) b) else [b]

let prime x = List.for_all (fun p -> x mod p != 0) (range 2 (x-1))

let check (x, y) =
  let not_prime x = not (prime x)
  in let p = x*y
  in let s = x+y
  in let zdanie_1 = not_prime p
  in let zdanie_2 = not_prime (s-1)
  in zdanie_1 && zdanie_2

let possible = List.filter check (List.concat (List.map (fun x -> List.map (fun y -> (x,y)) (range (x+1) 100)) (range 1 99)))

let rec uniq ls = match ls with
  | [] -> []
  | (x::xs) -> let
    maybe_x = if List.for_all (fun y -> x != y) xs then [x] else []
    in maybe_x @ (uniq xs)


