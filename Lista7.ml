(* Exercise 1 *)

let fix : 'a. (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = fun f a ->
  let rec r =
    fun ap ->
      f r ap
  in f r a

let s_fix : int -> int = fix (fun f -> fun n -> if n = 0 then 1 else n * (f (n-1)))

let s_for : int -> int = fun n ->
  let il = ref 1 in
  for i = 1 to n do
    il := !il * i
  done; !il

let fix_for : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b = fun f a ->
  let acc = ref (fun _ -> failwith "empty stack") in
  let r ap = f (!acc) ap in
  acc := r; f r a

let s_fix_for : int -> int = fix_for (fun f -> fun n -> if n = 0 then 1 else n * (f (n-1)))

(* Exercise 2 *)
type 'a list_mutable = LMnil | LMcons of 'a * 'a list_mutable ref

let concat_copy : 'a list_mutable -> 'a list_mutable -> 'a list_mutable =
  let rec aux a b = match a with
    | LMnil -> b
    | LMcons(x, xs) -> LMcons(x, ref(aux (!xs) b))
  in aux

let concat_share : 'a list_mutable -> 'a list_mutable -> 'a list_mutable =
  let rec aux a b = match a with
    | LMnil -> assert false
    | LMcons(_, xs) -> match !xs with
      | LMnil -> xs := b
      | LMcons(_) -> aux (!xs) b
  in fun a b -> aux a b; a

let list1_last = ref LMnil
let list1 = LMcons(1, ref(LMcons(2, list1_last)))
let list2 = LMcons(3, ref(LMcons(4, ref LMnil)))

(* Exercise 3 *)
type ('a, 'b) memoized = ('a * 'b) list

let empty_memoized : ('a, 'b) memoized = []

let find_memoized : ('a, 'b) memoized -> 'a -> 'b option = fun li a ->
  match List.filter (fun (ap, _) -> a = ap) li with
  | [(_, b)] -> Some b
  | [] -> None
  | _ -> assert false

let add_memoized : ('a, 'b) memoized -> 'a * 'b -> ('a, 'b) memoized =
  let rec aux = fun li (a, b) ->
    match li with
    | [] -> [(a,b)]
    | (ap, bp)::lip -> if a = ap then (a,b)::lip else (ap, bp)::(aux lip (a, b))
  in aux

let fib : int -> int =
  let rec aux k n1 n2 = match k with
    | 0 -> n2
    | 1 -> n1
    | _ -> aux (k-1) (n1+n2) n1
  in  fun n -> aux n 1 0

let fib_memo : int -> int =
  let acc : (int, int) memoized ref = ref @@ List.fold_left add_memoized empty_memoized [(0,0); (1,0)] in
  let rec aux n = match find_memoized (!acc) n with
    | None -> let new_value = aux (n-1) + aux (n-2) in acc := add_memoized (!acc) (n, new_value); new_value
    | Some a -> a
  in aux

let benchmark_func f = let
  cur = Sys.time () and _ = f () in Sys.time () -. cur

let rec range a b = if a = b then [b] else a::(range (a+1) b)
let benchmark_dom = fun _ -> (range 1000 10000)
let benchmark_fib = let dom = benchmark_dom () in
  benchmark_func (fun _ -> List.map fib dom)
let benchmark_fib_memo = let dom = benchmark_dom () in
  benchmark_func (fun _ -> List.map fib dom)

(* Exercise 4 *)

module type VarGen = sig
  val fresh : string -> string
  val reset : unit -> unit
end

module CustomVarGen : VarGen = struct
  let acc = ref 0

  let fresh s = acc := (!acc) + 1; s ^ (string_of_int (!acc))

  let reset = fun _ -> acc := 0
end

open CustomVarGen
