(* Task1 *)
val sublist : 'a list -> 'a list list

(* Task 2 *)
val a_nth : int -> int
val a_nth_tail : int -> int

(* Given argument, output times for each version *)
(* 1st element -> normal version, 2nd element -> tail version *)
val benchmark_a_nth : 'int -> float list
(* Example benchmark: *)
(* # benchmark_a_nth 10;; *)
(* - : float list = [0.; 0.] *)
(* # benchmark_a_nth 20;; *)
(* - : float list = [0.; 0.] *)
(* # benchmark_a_nth 30;; *)
(* - : float list = [0.0633330000000000282; 0.] *)
(* # benchmark_a_nth 40;; *)
(* - : float list = [7.38333299999999948; 0.] *)
(* Clearly, tail version is better *)

(* Task 3 *)
type map_type = ('a -> 'b) -> 'a list -> 'b list
val reverse_map : map_type 
val reverse_map_tail : map_type

(* Task 4 *)
type merge_type = ('a -> 'a -> bool) -> 'a list * 'a list -> 'a list
val merge : merge_type
val merge_tail : merge_type

(* Given argument, output times for each version *)
val benchmark_merge : int list * int list -> float list
(* Use range to generate arguments *)
val range : int -> int -> int list						   
(* Example benchmark: *)
(* # benchmark_merge (range 0 200, range 0 200);; *)
(* - : float list = [0.; 0.] *)
(* # benchmark_merge (range 0 2000, range 0 2000);; *)
(* - : float list = [0.; 0.] *)
(* # benchmark_merge (range 0 4000, range 0 4000);; *)
(* - : float list = [0.; 0.] *)
(* # benchmark_merge (range 0 01000, range 0 10000);; *)
(* - : float list = [0.; 0.] *)
(* # benchmark_merge (range 0 10000, range 0 10000);; *)
(* - : float list = [0.00666599999999999832; 0.] *)
(* # benchmark_merge (range 0 10000, range 0 50000);; *)
(* - : float list = [0.; 0.] *)
(* # benchmark_merge (range 0 50000, range 0 50000);; *)
(* Stack overflow during evaluation (looping recursion?). *)
(* merge_tail (<=) (range 0 60000, range 0 60000);; *)
(* - : int list = *)
(* [0; ...] *)
(* Looks like main advantage of tail version is that it doesn't overflow stack *)

(* Task 5 *)
val perm 'a list -> 'a list list

(* Task 6 *)
val suffs 'a list -> 'a list list
val preffs 'a list -> 'a list list
