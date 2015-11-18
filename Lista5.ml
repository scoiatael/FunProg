(* Exercise 1 *)
module type Stream = sig
  type 'a stream
  val empty : 'a stream
  val push : 'a -> (unit -> 'a stream) -> 'a stream
  val smap : ('a -> 'b) -> 'a stream -> 'b stream
  val stake : int -> 'a stream -> 'a list
  val sfrom : ('a -> 'a) -> 'a -> 'a stream
  val szip3 : 'a stream * 'b stream * 'c stream -> ('a * 'b * 'c) stream
  val stail : 'a stream -> 'a stream
  val sfilter : ('a -> bool) -> 'a stream -> 'a stream
end

let inc x = x + 1

module CustomStream = struct
  type 'a stream = Nil | Cons of ('a * (unit -> 'a stream))

  let empty = Nil
  let push a s = Cons(a,s)

  let rec sfilter f = function
    | Nil -> Nil
    | Cons(a, next) -> let fnext = fun () -> sfilter f (next ()) in
      if f a
      then Cons(a, fnext)
      else  fnext()

  let rec smap f = function
    | Nil -> Nil
    | Cons(a, next) -> Cons(f a, fun () -> smap f (next()))

  let rec sappend s1 s2 = match s1 with
    | Nil -> s2
    | Cons(a, next) -> Cons(a, fun () -> sappend (next ()) s2)

  let rec sfind f = function
    | Nil -> None
    | Cons(a, next) -> if f a then Some a else sfind f (next())

  let rec stake n s = if n > 0
    then
      match s with
      | Cons(a, c) -> a :: stake (n-1) (c())
      | _ -> assert false
    else
      []

  let stail = function
    | Cons(_, u) -> u()
    | _ -> assert false

  let rec sfrom f n = Cons(n, fun () -> sfrom f (f n))

  let rec slength = let
    rec aux n = function
      | Cons(_, u) -> aux (n+1) (u())
      | _ -> n
    in aux 0

  let list_of_stream s = stake (slength s) s

  let rec sfoldl f a s = List.fold_left f a (list_of_stream s)

  let rec szip3 : 'a stream * 'b stream * 'c stream -> ('a * 'b * 'c) stream = function
    | (Nil, _, _) -> Nil
    | (_, Nil, _) -> Nil
    | (_, _, Nil) -> Nil
    | (Cons(a1, u1), Cons(a2, u2), Cons(a3, u3)) -> Cons((a1,a2,a3), fun () -> szip3(u1(), u2(), u3()))
end

module Pi(M : Stream) = struct
  open M

  let imprecise_leibniz_stream : float stream =
    let aux n = let
      factor = float_of_int(if n mod 2 = 0 then 1 else -1)
      in factor *. (1. /. (2. *. float_of_int(n) +. 1.))
    in
    smap aux @@ sfrom inc 0

  let sum_pi_stream s = smap (fun n -> List.fold_left (+.) 0. (stake n s)) (sfrom inc 0)

  let imprecise_leibniz_pi4_stream = sum_pi_stream imprecise_leibniz_stream

  let smult4 = smap (fun x -> 4. *. x)

  let imprecise_leibniz_pi_stream = smult4 imprecise_leibniz_pi4_stream

  let precise_leibniz_pi4_stream : float stream =
    let reszipf : ('a -> 'a -> 'a -> 'b) -> 'a stream -> 'b stream =
      fun f s ->
        smap (fun (s1, s2, s3) -> f s1 s2 s3) (szip3 (s, stail s, stail @@ stail s))
    in let euler x y z = z -. (y -. z)**2. /. (x -. 2. *. y +. z)
    in reszipf euler imprecise_leibniz_pi4_stream

  let precise_leibniz_pi_stream = smult4 precise_leibniz_pi4_stream
end

module CustomPi = Pi(CustomStream)
(* Using lazy *)

module LazyStream = struct
  open Lazy

  type 'a stream = Nil | Cons of ('a * 'a stream Lazy.t)

  let empty = Nil
  let push a s = Cons(a, lazy (s()))

  let rec sfilter f = function
    | Nil -> Nil
    | Cons(a, next) -> let fnext = lazy (sfilter f (force next)) in
      if f a
      then Cons(a, fnext)
      else force fnext

  let rec smap f = function
    | Nil -> Nil
    | Cons(a, next) -> Cons(f a, lazy (smap f (force next)))

  let rec sappend s1 s2 = match s1 with
    | Nil -> s2
    | Cons(a, next) -> Cons(a, lazy (sappend (force next) s2))

  let rec sfind f = function
    | Nil -> None
    | Cons(a, next) -> if f a then Some a else sfind f (force next)

  let rec stake n s = if n > 0
    then
      match s with
      | Cons(a, c) -> a :: stake (n-1) (force c)
      | _ -> assert false
    else
      []

  let stail = function
    | Cons(_, u) -> force u
    | _ -> assert false

  let rec sfrom f n = Cons(n, lazy (sfrom f (f n)))

  let rec slength = let
    rec aux n = function
      | Cons(_, u) -> aux (n+1) (force u)
      | _ -> n
    in aux 0

  let list_of_stream s = stake (slength s) s

  let rec sfoldl f a s = List.fold_left f a (list_of_stream s)

  let rec szip3 : 'a stream * 'b stream * 'c stream -> ('a * 'b * 'c) stream = function
    | (Nil, _, _) -> Nil
    | (_, Nil, _) -> Nil
    | (_, _, Nil) -> Nil
    | (Cons(a1, u1), Cons(a2, u2), Cons(a3, u3)) -> Cons((a1,a2,a3), lazy (szip3(force u1, force u2, force u3)))
end

module LazyPi = Pi(LazyStream)

(* Exercise 2 *)
module Queen(M : Stream) = struct
  open M

  let bfs next x =
    let rec aux = function
      | [] -> empty
      | (h::t) -> push h @@ fun () -> aux (t @ next h)
    in aux [x]

  let dfs next x =
    let rec aux = function
      | [] -> empty
      | (h::t) -> push h @@ fun () -> aux (next h @ t)
    in aux [x]

  let isQueenSafe oldqs newq =
    let rec nodiag = function
      | (i, [])  -> true
      | (i, q::qt) -> abs(newq-q)<>i && nodiag(i+1,qt)
    in not(List.mem newq oldqs) && nodiag(1,oldqs)

  let rec fromTo  a b =
    if a>b then []
    else a::(fromTo (a+1) b)

  let nextQueen n qs =
    List.map (function h -> h::qs)
      (List.filter (isQueenSafe qs) (fromTo 1 n))

  let isSolution n qs = List.length qs = n

  let depthQueen n = sfilter (isSolution n)
      (dfs (nextQueen n) [])

  let breadthQueen n = sfilter (isSolution n)
      (bfs (nextQueen n) [])

  let optBreadthQueen n =
    let check = isSolution n in
    let generate = nextQueen n in
    let rec aux acc = function
      | [] -> acc
      | (h::t) -> let (s, m) = if check h then ([h], []) else ([], generate h) in
        aux (acc @ s) (t @ m) in
    aux [] [[]]
end

module CustomQueen = Queen(CustomStream)
module LazyQueen = Queen(LazyStream)



(* Exercise 4 *)

type frequency = int

type 'a huffTree = Leaf of ('a * frequency) | Node of ('a huffTree * frequency * 'a huffTree)

let freq  = function
  | Leaf(_, f) -> f
  | Node (_, f, _) -> f

let join t1 t2 = Node(t1, freq t1 + freq t2, t2)

let sortAssocList = List.sort (fun t1 t2 -> compare (freq t1) (freq t2))

let makeHuffTree =
  let slurp s = match sortAssocList s with
    | (x::y::li) -> (join x y, li)
    | [x] -> (x, [])
    | [] -> assert false
  in let rec aux s = match slurp s with
      | (n, []) -> n
      | (n, li) -> aux (n::li)
  in aux

let encode tr =
  let rec encode_sym s = function
    | Leaf(x, _) -> if s = x then Some [] else None
    | Node(ls, _, rs) -> match encode_sym s ls with
      | Some st -> Some (0::st)
      | None -> match encode_sym s rs with
        | Some st -> Some (1::st)
        | None -> None
  in let get_some = function
      | Some s -> s
      | _ -> failwith "Element not found in tree"

  in
  List.map (fun s -> get_some @@ encode_sym s tr)

let decode tr =
  let get_leaf_value = function
    | Leaf(x, _) -> x
    | _ -> failwith "Bad code given"

  in let get_lefthand_side = function
      | Node(l, _, _) -> l
      | _ -> failwith "Bad code given"

  in let get_righthand_side = function
      | Node(_, _, r) -> r
      | _ -> failwith "Bad code given"

  in let rec decode_sym tr = function
      | [] -> get_leaf_value tr
      | (0::li) -> decode_sym (get_lefthand_side tr) li
      | (1::li) -> decode_sym (get_righthand_side tr) li
      | _ -> failwith "Bad code given"
  in List.map decode_sym

let tree_of_string s =
  let push_elem li e = match li with
    | [] -> [(e, 1)]
    | ((p, n)::es) -> (if e = p then [(p, n+1)] else [(e, 1); (p, n)]) @ es
  in let assoc = List.fold_left push_elem [] @@ List.sort compare s
  in makeHuffTree @@ List.map (fun (a, f) -> Leaf(a,f)) assoc
