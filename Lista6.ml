(* Exercise 1 *)
type 'a btree = Leaf of 'a | Node of 'a btree * 'a btree

let rec flatten_btree : 'a btree -> 'a list = function
  | Leaf a -> [a]
  | Node(l,r) -> flatten_btree l @ flatten_btree r

let samefringe : 'a btree -> 'a btree -> bool = fun t1 t2 -> flatten_btree t1 = flatten_btree t2

open Lazy

type 'a stream = Nil | Cons of ('a * 'a stream Lazy.t)

let rec sappend s1 s2 = match s1 with
| Nil -> force s2
| Cons(a, next) -> Cons(a, lazy (sappend (force next) s2))

let rec lflatten_btree : 'a btree -> 'a stream =  function
  | Leaf a -> Cons(a, lazy Nil)
  | Node(l,r) -> sappend (lflatten_btree l) (lazy (lflatten_btree r))

let samefringe_opt : 'a btree -> 'a btree -> bool = fun t1 t2 -> lflatten_btree t1 = lflatten_btree t2

(* Exercise 2 *)
type 'a vbtree = VLeaf of 'a | VNode of 'a vbtree * 'a * 'a vbtree

let enum_dfs : 'a vbtree -> int vbtree =
  let rec aux i = function
    | VLeaf(_) -> (VLeaf(i), i + 1)
    | VNode(l, _, r) ->
      let (lp, il) = aux (i+1) l
      in
      let (rp, ir) = aux il r
      in
      (VNode(lp, i, rp), ir)
  in fun t ->
    let (t_enum, _) = aux 1 t in t_enum

let some_vbtree = VNode (VNode (VLeaf 'a', 'b', VLeaf 'c'), 'd', VLeaf 'e')

let rec from a b = if a < b then a::(from (a+1) b) else [b]

let inc n = n + 1

let enum_bfs : 'a vbtree -> int vbtree =
  let rec aux = function
    | (s, []) -> (s, [])
    | (s, VLeaf(_)::ts) -> let (sp, li) = aux (inc s, ts) in (sp, li @ [VLeaf(s)])
    | (s, VNode(l, _, r)::ts) -> let (sp, li) = aux (inc s, ts @ [l; r]) in match li with
      | (rp::lp::lip) -> (sp, lip @ [VNode(lp, s, rp)])
      | _ -> assert false

  in fun t -> match aux (1, [t]) with
    | (_, [tp]) -> tp
    | _ -> assert false

(* Exercise 3 *)

type 'a array = int * 'a btree option

let aempty : 'a array = (0, None)

let rec asub : 'a. 'a array -> int -> 'a =
  let rec aux i = function
    | Leaf a -> a
    | Node(l, r) ->
      if i mod 2 = 0
      then aux (i / 2) l
      else aux (i / 2) r
  in
  fun (k, tree) i ->
    if k > i
    then match tree with
      | Some t -> aux i t
      | _ -> assert false
    else assert false

let aupdate : 'a array -> int -> 'a -> 'a array =
  let rec aux i e = function
    | Leaf(_) -> Leaf(e)
    | Node(l, r) ->
      if i mod 2 = 0
      then Node((aux (i / 2) e l), r)
      else Node(l, aux (i / 2) e r)
  in fun (k, tree) i e ->
    if k > i
    then match tree with
      | Some t -> (k, Some(aux i e t))
      | _ -> aempty
    else assert false

let ahiext : 'a array -> 'a -> 'a array =
  let rec aux i e = function
    | Leaf a -> Node(Leaf(a), Leaf(e))
    | Node(l, r) -> let (lp, rp) =
                      if i mod 2 = 0
                      then (aux (i / 2) e l, r)
                      else (l, aux (i / 2) e r)
      in Node(lp, rp)
  in fun (k, tree) e ->
    match tree with
      | Some t -> (k+1, Some (aux k e t))
      | None -> (1, Some(Leaf(e)))

let ahirem : 'a array -> 'a array =
  let rec aux : int -> 'a btree -> 'a btree = fun i t ->
    let even = i mod 2 = 0 in
    match (t, i < 2) with
    | (Node(l, r), true) -> if even then r else l
    | (Node(l, r), false) -> let (lp, rp) =
                               if even
                               then (aux (i / 2) l, r)
                               else (l, aux (i / 2) r)
      in Node(lp, rp)
    | _ -> assert false
  in fun (k, tree) -> if k == 1
    then aempty
    else
      match tree with
      | Some t -> (k-1, Some (aux (k-1) t))
      | _ -> assert false

(* Zadanie 4 *)

type 'a sprintf_cont = string -> 'a
type ('b, 'a) sprintf_resolv = string -> ('b -> 'a)

let append_cont
  : ('b -> string) -> 'a sprintf_cont -> ('b, 'a) sprintf_resolv =
  fun f cont str b -> cont (str ^ f b)

let inr : 'a sprintf_cont -> (int, 'a) sprintf_resolv =
  append_cont string_of_int

let eol : 'a sprintf_cont -> string -> 'a =
  fun con str -> con (str ^ "\n")

let lit : string -> 'a sprintf_cont -> string -> 'a =
  fun s1 con str -> con (str ^ s1)

let flt : 'a sprintf_cont -> (float, 'a) sprintf_resolv =
  append_cont string_of_float

let str : 'a sprintf_cont -> (string, 'a) sprintf_resolv =
  append_cont ((^) "")

let (++) f1 f2 c = f1 @@ f2 c

let sprintf f = f print_string @@ ""
