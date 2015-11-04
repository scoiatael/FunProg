let is_palindrome : 'a list -> bool = function xs ->
  let rec fast_rev acc (x::xs) = function
    | []
    | [_] -> (acc, x::xs)
    | (_::_::zs) -> fast_rev (x::acc) xs zs
  in let (h1,h2) = (fast_rev [] xs xs) in h1 = h2


type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree

let is_balanced : 'a btree -> bool =
  let rec aux : 'a btree -> int option = function
    | Leaf -> Some 0
    | Node(ls, _, rs) -> match (aux ls, aux rs) with
      | (None, _) -> None
      | (_, None) -> None
      | (Some lh, Some rh) -> let diff = abs(lh - rh) in if diff > 1 then None else Some(1 + max lh rh)
  in fun tr -> match aux tr with
    | None -> false
    | Some _ -> true

let partition : int -> 'a list -> 'a list * 'a list = function n ->
  let counter = ref n in List.partition (fun _ -> counter := !counter - 1; !counter >= 0)

let build_balanced : 'a list -> 'a btree =   let rec aux xs l = match l with
    | 0 -> Leaf
    | 1 -> (match xs with
        | [x] -> Node(Leaf, x, Leaf)
        | _ -> assert false)
    | n -> (match xs with
        | (x::xs) -> let n1 = n - 1 in let n1_half = n1/2 in let (ls, rs) = partition n1_half xs in Node(aux ls n1_half, x, aux rs (n1 - n1_half))
        | _ -> assert false)
  in function ls ->
    let len = List.length ls in aux ls len

type 'a mtree = MNode of 'a * 'a forest and 'a forest = EmptyForest | Forest of 'a mtree * 'a forest
let bfs_mtree : 'a mtree -> 'a list =
  let rec aux_forest = function
    | EmptyForest -> []
    | Forest(atree, aforest) -> atree :: aux_forest aforest in
  let rec aux_mtree = function nodes ->
    let (sons, branches) = List.split @@ List.map (fun (MNode(a,af)) -> (a,af)) nodes in sons @ (List.concat @@ List.map aux_mtree @@ List.map aux_forest branches)
  in fun atree -> aux_mtree [atree]

let dfs_mtree : 'a mtree -> 'a list =
  let rec aux_forest = function
    | EmptyForest -> []
    | Forest(atree, aforest) -> (aux_mtree atree) @ (aux_forest aforest)
  and aux_mtree (MNode(a, aforest)) = a :: (aux_forest aforest)
  in aux_mtree

let big_mtree = MNode(1, Forest(MNode(2, Forest(MNode(3, EmptyForest), EmptyForest)), Forest(MNode(4,EmptyForest), Forest(MNode(5, EmptyForest), EmptyForest))))

type 'a mtree_list = MTree of 'a * ('a mtree_list) list

let bfs_mtree_list : 'a mtree_list -> 'a list =
  let rec aux_mtree = function nodes ->
    let (sons, branches) = List.split @@ List.map (fun (MTree(a, af)) -> (a,af)) nodes in sons @ (List.concat @@ List.map aux_mtree branches)
  in fun atree -> aux_mtree [atree]

let dfs_mtree_list : 'a mtree_list -> 'a list =
  let rec aux_mtree = function (MTree(a, af)) -> a :: (List.concat @@ List.map aux_mtree af)
  in aux_mtree

let big_mtree_list = MTree(1, [ MTree(2, [MTree(3, [])]); MTree(4, []); MTree(5, []) ])

type zero_ord_log = Var of string | Neg of zero_ord_log | Disj of zero_ord_log list | Conj of zero_ord_log list
type interpretation = string -> bool

let rec free_vars : zero_ord_log -> string list = function
  | Var s -> [s]
  | Neg x -> free_vars x
  | Conj x
  | Disj x -> List.concat @@ List.map free_vars x

let rec eval : interpretation -> zero_ord_log -> bool = fun f ->
  function Var x -> f x
         | Neg x -> not (eval f x)
         | Disj x -> List.fold_left (fun v zol -> v || (eval f zol)) false x
         | Conj x -> List.fold_left (fun v zol -> v && (eval f zol)) true x

let add_val f x v = fun y -> if x = y then v else f y

type 'a stream = Nil | Cons of ('a * (unit -> 'a stream))
let rec smap f = function
  | Nil -> Nil
  | Cons(a, next) -> Cons(f a, fun () -> smap f (next()))

let rec sappend s1 s2 = match s1 with
  | Nil -> s2
  | Cons(a, next) -> Cons(a, fun () -> sappend (next ()) s2)

let rec sfind f = function
  | Nil -> None
  | Cons(a, next) -> if f a then Some a else sfind f (next())

let rec all_interpretations : string list -> interpretation stream = function
  | [] -> Cons((fun x -> failwith @@ "Variable " ^ x ^ " not bound"), fun () -> Nil)
  | x :: xs -> let ins = all_interpretations xs in let pos f = add_val f x true in let neg f = add_val f x false in
    sappend (smap pos ins) (smap neg ins)

let is_taut : zero_ord_log -> interpretation option = fun zol ->
  sfind (fun i -> not (eval i zol)) (all_interpretations @@ free_vars zol)

let basic_taut = Disj[Var "X"; Neg(Var "X")]

let test_basic_taut = match is_taut basic_taut with
  | None -> None
  | Some i -> Some (i "X")

let nnf : zero_ord_log -> zero_ord_log =
  let rec aux neg = function
    | Var x -> if neg then Neg (Var x) else Var x
    | Neg x -> if neg then aux false x else aux true x
    | Disj x -> let xp = List.map (aux neg) x in if neg then Conj xp else Disj xp
    | Conj x -> let xp = List.map (aux neg) x in if neg then Disj xp else Conj xp
  in aux false

let normalize : zero_ord_log -> zero_ord_log =
  let rec flatten_conj = function
    | Conj x -> List.concat @@ List.map flatten_conj x
    | x -> [x] in
  let rec flatten_disj = function
    | Disj x -> List.concat @@ List.map flatten_disj x
    | x -> [x] in
  let rec aux = function
    | Neg x -> Neg(aux x)
    | Disj [x] -> x
    | Disj x -> Disj( List.map aux @@ List.concat @@ List.map flatten_disj x )
    | Conj [x] -> x
    | Conj x -> Conj( List.map aux @@ List.concat @@ List.map flatten_conj x )
    | x -> x
  in fun x -> nnf @@ aux x


(*  (P and Q) or V or X <=> ((P or V) and (Q or V)) or X <=> (P or V or X) and (Q or V or X) *)
let cnf : zero_ord_log -> zero_ord_log = fun _ -> Var "NYI"

let basic_term = Disj([Var "X"; Var "V"; Conj([Var "P"; Var "Q"])])

let advanced_term = Disj([Disj([Var "X"; Neg (Var "Y")]); Neg(Disj([Var "Z"; Var "Y"]))])

let prod : int btree -> int =
  let rec aux : (int -> unit) -> int btree -> unit = fun c -> function
    | Leaf -> c 1
    | Node (ls,a,rs) -> aux (fun lp -> aux (fun rp -> c(lp * rp * a)) rs) ls
  let ret = ref 0 in
  in fun tree -> aux (fun v -> ret := v) tree; !ret

let better_prod : int btree -> int =
  let ret = ref 0 in
  let rec aux : (int -> unit) -> int btree -> unit = fun c -> function
    | Leaf -> c 1
    | Node (ls,a,rs) -> if a = 0 then c 0 else aux (fun lp -> aux (fun rp -> c(lp * rp * a)) rs) ls
  in fun tree -> aux (fun v -> ret := v) tree; !ret

let some_tree = Node(Leaf, 1, Node(Leaf, 5, Node(Leaf, 5, Leaf)))
