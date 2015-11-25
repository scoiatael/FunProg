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

(* Exercise 4 *)

type 'a sprintf_cont = string -> 'a
type ('b, 'a) sprintf_resolv = string -> ('b -> 'a)

let append_cont
  : ('b -> string) -> 'a sprintf_cont -> ('b, 'a) sprintf_resolv =
  fun f cont str b -> cont (str ^ f b)

let inr : 'a sprintf_cont -> (int, 'a) sprintf_resolv =
  append_cont string_of_int

let flt : 'a sprintf_cont -> (float, 'a) sprintf_resolv =
  append_cont string_of_float

let str : 'a sprintf_cont -> (string, 'a) sprintf_resolv =
  append_cont ((^) "")

let eol : 'a sprintf_cont -> string -> 'a =
  fun con str -> con (str ^ "\n")

let lit : string -> 'a sprintf_cont -> string -> 'a =
  fun s1 con str -> con (str ^ s1)

let (++) f1 f2 c = f1 @@ f2 c

let sprintf f = f print_string @@ ""

(* Excercise 5 *)
(* prolog.ml *)

(* An atom is either a propositional variable or an alternative of two goals. *)
type atom =
  | Atom of string
  | Or of goal * goal
(* A goal is a list of atoms. *)
and goal = atom list
(* A clause consists of a head (a propositional variable) and a body (a goal). *)
type clause = string * goal
(* A Prolog program is a list of clauses. *)
type program = clause list

(* Search a program for a clause with a given head. *)
let rec lookup x pgm =
  match pgm with
    | [] ->
      None
    | (y, g) :: p ->
      if x = y then Some g else lookup x p

(*
A propositional Prolog interpreter written in CPS with two layers of continuations:
a success and a failure continuation. The failure continuation is parameterless and
it specifies what should happen next in case of a failure in the current goal. The
success continuation takes a failure continuation as an argument and it specifies
what should happen next in case the current goal is satisfied.
*)

(*      eval_atom : atom -> program -> ((unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a *)
let rec eval_atom a p sc fc =
  match a with
  | Atom x ->
    (match (lookup x p) with
     | None ->
       fc ()
     | Some g ->
       eval_goal g p sc fc)
  | Or (g1, g2) ->
    eval_goal g1 p sc (fun () -> eval_goal g2 p sc fc)

(*  eval_goal : goal -> program -> ((unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a  *)
and eval_goal g p sc fc =
  match g with
    | [] ->
      sc fc
    | a :: g ->
      eval_atom a p (fun fc' -> eval_goal g p sc fc') fc

(*  run : goal ->  program -> bool  *)
(* To make this interpreter tail recursive, change unit to arbitrary state *)
let run g p = eval_goal g p (fun fc -> 1 + fc ()) (fun () -> 0)

(* tests *)

let p1 = [("a", [Atom "b"; Atom "c"]);
    ("b", [])]

let p2 = [("a", [Atom "b"; Or ([Atom "c"], [Atom "d"]); Atom "e"]);
    ("b", [Atom "d"]);
    ("d", []);
    ("e", [Atom "d"])]

let p3 = [("a", [Atom "b"; Or ([Atom "c"], [Atom "d"]); Atom "e"]);
    ("b", [Atom "d"]);
    ("c", []);
    ("d", []);
    ("e", [Atom "d"])]

let g1 = [Atom "a"]

let v1_1 = run g1 p1
let v1_2 = run g1 p2
let v1_3 = run g1 p3

(* eof *)

type regexp =
  | Atom of char
  | And of regexp * regexp
  | Or of regexp * regexp
  | Star of regexp

type 'a failure_cont = unit -> 'a
let rec match_regexp
  : regexp -> char list -> (char list -> 'a failure_cont -> 'a) -> 'a failure_cont -> 'a
  = fun r chs sc fc -> match r with
    | Atom(a) -> (match chs with
        | (c::chsr) -> if a = c then sc chsr fc else fc ()
        | _ -> fc ())
    | And(a,b) -> match_regexp a chs (fun chsr nfc -> match_regexp b chsr sc nfc) fc
    | Or(a,b) -> match_regexp a chs sc (fun _ -> match_regexp b chs sc fc)
    | Star(a) ->
      let recover_fc = fun nchs _ -> sc nchs fc in
      let rec greedy_sc = fun nchs nfc -> match_regexp a nchs greedy_sc (recover_fc nchs) in
      match_regexp a chs greedy_sc (recover_fc chs)

let run : regexp -> char list -> bool =
  fun r chs -> match_regexp r chs (fun nchs nfc -> if List.length nchs == 0 then true else nfc ()) (fun _ -> false)

(* tests *)

let p1 = And(Star(Atom('a')), Atom('b'))
let g11 = ['a';'a';'a';'b']
let g12 = ['b']
let g13 = ['a']

let p2 = Or(Atom('a'), Atom('b'))
let g21 = ['a']
let g22 = ['b']
let g23 = ['a';'b']

let v1_1 = run p1 g11
let v1_2 = run p1 g12
let v1_3 = run p1 g13

let v2_1 = run p2 g21
let v2_2 = run p2 g22
let v2_3 = run p2 g23
