(* Exercise 1 *)

(* i. *)
module type PQUEUE =
sig
 type priority
 type 'a t

 exception EmptyPQueue

 val priority_of_int : int -> priority
 val empty : 'a t
 val is_empty : 'a t -> bool
 val insert : 'a t -> priority -> 'a -> 'a t
 val remove : 'a t -> priority * 'a * 'a t
end

module PQueue : PQUEUE =
struct
  type priority = int

  type 'a t = (priority * 'a) list

  exception EmptyPQueue

  let empty = []
  let insert q p a = (p,a)::q
  let remove q =
    let cmp (p1, a1) (p2, a2) = compare p1 p2 in
    match List.sort cmp q with
    | [] -> raise EmptyPQueue
    | (p,x)::xs -> (p, x, xs)
  let is_empty = function
    | [] -> true
    | _ -> false
  let priority_of_int x = x
end

(* ii. *)
let sort_int : int list -> int list =
  let open PQueue in
  let rec aux : int t -> int list = fun q -> if is_empty q
    then []
    else let (p, x, qp) = remove q in x::(aux qp)
  in
  fun xs  -> aux @@ List.fold_left (fun q x -> insert q (priority_of_int x) x) empty xs

module type ORDTYPE =
sig
  type t

  val compare : t -> t -> int
  val t_of_int : int -> t
end

(* iii. *)
module PQueueOrd(OrdType : ORDTYPE) : PQUEUE =
struct
  type priority = OrdType.t

  type 'a t = (priority * 'a) list

  exception EmptyPQueue

  let empty = []
  let insert q p a = (p,a)::q
  let remove q =
    let cmp (p1, a1) (p2, a2) = OrdType.compare p1 p2 in
    match List.sort cmp q with
    | [] -> raise EmptyPQueue
    | (p,x)::xs -> (p, x, xs)
  let is_empty = function
    | [] -> true
    | _ -> false
  let priority_of_int x = OrdType.t_of_int x
end

let sort_int_ord : int list -> int list =
  let module IntOrd : ORDTYPE =
  struct
    type t = int
    let compare = compare
    let t_of_int x = x
  end in
  let module PQ =  PQueueOrd(IntOrd) in
  let open PQ in
  let rec aux : int t -> int list = fun q -> if is_empty q
    then []
    else let (p, x, qp) = remove q in x::(aux qp)
  in
  fun xs  -> aux @@ List.fold_left (fun q x -> insert q (priority_of_int x) x) empty xs


let int_list = [9;2;1;3;545;1;67;2]

(* Exercise 2 *)

module type VERTEX =
sig
  type t
  type label

  val equal : t -> t -> bool
  val create : label -> t
  val label : t -> label
end

(* i. *)

module type EDGE =
sig
  type t
  type vertex
  type label

  val equal : t -> t -> bool
  val create : label -> vertex -> vertex -> t
  val label : t -> label
  val start : t -> vertex
  val finish : t -> vertex
end

(* ii. *)

module Vertex : VERTEX with type label = string =
struct
  type label = string
  type t = label * unit ref

  let equal t1 t2 = t1 = t2
  let create l = (l, ref ())
  let label (l, _) = l
end

module Edge : EDGE with type vertex = Vertex.t
                    and type label = string =
struct
  type vertex = Vertex.t
  type label = string

  type t = vertex * label * vertex * unit ref

  let equal t1 t2 = t1 = t2
  let create l v1 v2 = (v1, l, v2, ref ())
  let label (_, l, _, _) = l
  let start (v, _, _, _) = v
  let finish (_, _, v, _) = v
end

(* iii. *)
module type GRAPH =
sig
  type t

  module V : VERTEX
  type vertex = V.t

  module E : EDGE with type vertex = vertex
  type edge = E.t

  val mem_v : t -> vertex -> bool
  val mem_e : t -> edge -> bool
  val mem_e_v : t -> vertex -> vertex -> bool
  val find_e : t -> vertex -> vertex -> edge
  val succ : t -> vertex -> vertex list
  val pred : t -> vertex -> vertex list
  val succ_e : t -> vertex -> edge list
  val pred_e : t -> vertex -> edge list

  val empty : t
  val add_e : t -> edge -> t
  val add_v : t -> vertex -> t
  val rem_e : t -> edge -> t
  val rem_v : t -> vertex -> t

  val fold_v : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
end

module Graph : GRAPH with module V = Vertex
                      and module E = Edge
                      and type edge = Edge.t
                      and type vertex = Vertex.t =
struct
  module V = Vertex
  type vertex = V.t

  module E = Edge
  type edge = E.t

  type t = vertex list * edge list

  let mem_v (vs, _) v = List.mem v vs
  let mem_e (_, es) e = List.mem e es

  let find_e_aux g v1 v2 = let (_, es) = g in
                           match List.find_all (fun e -> E.start e = v1 && E.finish e = v2) es with
                           | [] -> None
                           | x::_ -> Some x
  let mem_e_v g v1 v2 = match find_e_aux g v1 v2 with
    | None -> false
    | Some e -> true
  let find_e g v1 v2 = match find_e_aux g v1 v2 with
    | None -> assert false
    | Some e -> e
  let succ_e (_, es) v = List.find_all (fun e -> E.start e = v) es
  let pred_e (_, es) v = List.find_all (fun e -> E.finish e = v) es
  let succ g v = List.map E.finish @@ succ_e g v
  let pred g v = List.map E.start @@ pred_e g v

  let empty  = ([], [])
  let add_e g e =
    let s = E.start e in
    let f = E.finish e in
    assert(mem_v g s); assert(mem_v g f);
    let (vs, es) = g in
    (vs, e::es)
  let add_v (vs, es) v = (v::vs, es)
  let rem_e (vs, es) e = (vs, List.filter (fun e1 -> not @@ E.equal e1 e) es)
  let rem_v (vs, es) v =
    let vsp = List.filter (fun v1 -> not @@ V.equal v1 v) vs in
    let esp = List.filter (fun e -> let (s,f) = (E.start e, E.finish e) in (not @@ V.equal s v) && (not @@ V.equal f v)) es in
    (vsp, esp)
  let fold_v f (vs, _) a = List.fold_left (fun a v -> f v a) a vs
  let fold_e f (_, es) a = List.fold_left (fun a v -> f v a) a es
end

(* iv. *)

(*
*         +---+                +---+
*         | 1 +--------------->+ 2 |
*         +-+-+               /+--++
*           |               /-    |
*           |            /--      |
*           |          /-         |
*           |        /-           |
*           |     /--             |
*           ⌄   /-                ⌄
*         +-+-+⤶                +-+-+
*         | 4-+<----------------+ 3 |
*         +---+                 +---+
*
*)

let vertices = List.map Vertex.create @@ List.map string_of_int [1;2;3;4]
let edges =
  let lines = [(0,3); (0,1); (1,2); (2,3); (1,3)]
  in let labels_start_finish = List.map (fun (a, b) -> (string_of_int a ^ "->" ^ string_of_int b, List.nth vertices a, List.nth vertices b)) lines
  in List.map (fun (a,b,c) -> Edge.create a b c) labels_start_finish

let graph =
  let g = Graph.empty
  in let vg = List.fold_left Graph.add_v g vertices
  in List.fold_left Graph.add_e vg edges

(* v. *)

module GraphFunc(Ve : VERTEX) (Ed : EDGE with type vertex = Ve.t) : GRAPH =
struct
  module V = Ve
  type vertex = V.t

  module E = Ed
  type edge = E.t

  type t = vertex list * edge list

  let mem_v (vs, _) v = List.mem v vs
  let mem_e (_, es) e = List.mem e es

  let find_e_aux g v1 v2 = let (_, es) = g in
                           match List.find_all (fun e -> E.start e = v1 && E.finish e = v2) es with
                           | [] -> None
                           | x::_ -> Some x
  let mem_e_v g v1 v2 = match find_e_aux g v1 v2 with
    | None -> false
    | Some e -> true
  let find_e g v1 v2 = match find_e_aux g v1 v2 with
    | None -> assert false
    | Some e -> e
  let succ_e (_, es) v = List.find_all (fun e -> E.start e = v) es
  let pred_e (_, es) v = List.find_all (fun e -> E.finish e = v) es
  let succ g v = List.map E.finish @@ succ_e g v
  let pred g v = List.map E.start @@ pred_e g v

  let empty  = ([], [])
  let add_e g e =
    let s = E.start e in
    let f = E.finish e in
    assert(mem_v g s); assert(mem_v g f);
    let (vs, es) = g in
    (vs, e::es)
  let add_v (vs, es) v = (v::vs, es)
  let rem_e (vs, es) e = (vs, List.filter (fun e1 -> not @@ E.equal e1 e) es)
  let rem_v (vs, es) v =
    let vsp = List.filter (fun v1 -> not @@ V.equal v1 v) vs in
    let esp = List.filter (fun e -> let (s,f) = (E.start e, E.finish e) in (not @@ V.equal s v) && (not @@ V.equal f v)) es in
    (vsp, esp)
  let fold_v f (vs, _) a = List.fold_left (fun a v -> f v a) a vs
  let fold_e f (_, es) a = List.fold_left (fun a v -> f v a) a es
end

(* vi. *)
module DFS (G : GRAPH) =
struct
  let visit g =
    let rec aux acc = function
      | [] -> List.rev acc
      | (h::t) ->
        let succs = Graph.succ g h in
        let next = List.filter (fun v -> not @@ List.mem v acc) succs in
          aux (h::acc) (next @ t)
    in fun x -> aux [] [x]
end

module BFS(G : GRAPH) =
struct
  let visit g =
    let rec aux acc = function
      | [] -> List.rev acc
      | (h::t) ->
        let succs = Graph.succ g h in
        let next = List.filter (fun v -> not @@ List.mem v acc) succs in
          aux (h::acc) (t @ next)
    in fun x -> aux [] [x]
end

module Dfs = DFS(Graph)

module Bfs = BFS(Graph)
