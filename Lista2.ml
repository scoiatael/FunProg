let rec sublist li = match li with
  | x :: xs -> let xss = sublist xs in (List.map (fun ls -> x :: ls) xss) @ xss
  | [] -> [[]]

let rec a_nth n = match n with
  | 0 -> 1
  | 1 -> 2
  | n -> 2 * (a_nth (n-2)) - a_nth (n-1) + 1

let a_nth_tail = let
    rec a_nth_tail2 (n1, n2) k = match k with
    | 0 -> n2
    | 1 -> n1
    | _ -> let
        n2p = n1
      and
        n1p = 2 * n2 - n1 + 1
      in
      a_nth_tail2 (n1p, n2p) (k-1)
  in
  a_nth_tail2 (2,1)

let benchmark_a_nth big_k = let
    benchmark_func f = let
      cur = Sys.time () and _ = f big_k in Sys.time () -. cur
  in
  List.map benchmark_func [a_nth; a_nth_tail]

let rec reverse_map f m = match m with
  | [] -> []
  | x :: xs -> (reverse_map f xs) @ [f x]

let reverse_map_tail f = let
    rec reverse_map_acc acc m = match m with
    | [] -> acc
    | x :: xs -> reverse_map_acc ((f x) :: acc) xs
  in reverse_map_acc []

let rec merge cmp = function
  | ([], l) -> l
  | (l, []) -> l
  | (x :: xs, y :: ys) -> if cmp x y
                          then
                            x :: (merge cmp (xs, (y::ys)))
                          else
                            y :: (merge cmp ((x :: xs), ys))
let merge_tail cmp = let
    rec merge_acc acc = function
    | ([], l) -> (List.rev acc ) @ l
    | (l, []) -> (List.rev acc ) @ l
    | (x :: xs, y :: ys) -> if cmp x y
                            then
                              merge_acc (x :: acc) (xs, (y :: ys))
                            else
                              merge_acc (y :: acc) ((x :: xs), ys)
  in merge_acc []

let rec merge_sort cmp = function
  | [] -> []
  | [x] -> [x]
  | big_list ->
    let
      rec partition = function
      | [] -> ([], [])
      | [x] -> ([x], [])
      | (x::y::zs) -> let (xs, ys) = partition zs in (x::xs, y::ys)
    in let
      (p1,p2) = partition big_list
    in merge_tail cmp (merge_sort cmp p1, merge_sort cmp p2)

let benchmark_merge lss = let benchmark_func f = let
                              cur = Sys.time ()
                            and
                              _ = f (<=) lss
                            in Sys.time () -. cur
                          in
                          List.map benchmark_func [merge; merge_tail]

let rec range a b = if a > b then [] else a :: (range (a+1) b)

let rec insert x = function
  | [] -> [[x]]
  | y :: ys -> (x :: y :: ys) :: List.map (fun l -> y :: l) (insert x ys)

let rec perm = function
  | [] -> [[]]
  | x :: xs -> let
      perms = perm xs
    in (List.concat (List.map (insert x) perms))

let rec suffs = function
  | [] -> [[]] (* Sane base to let me sleep in peace *)
  | [x] -> [[x]]
  | x -> x :: (suffs (List.tl x))

let rec preffs = function
  | [] -> []
  | x -> let
      h = List.hd x
    and
      t = List.tl x
    in
    List.map (fun l -> h :: l) ([] :: preffs t)
