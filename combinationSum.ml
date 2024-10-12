(* Combination Sum for a Sequence of Distinct Integers & a target sum *)

(* Utility functions *)
let length l = 
  let rec len l acc = match l with
    | [] -> acc
    | x::t -> len t (acc+1)
  in len l 0;;

let rev l = 
  let rec rev' l acc = match l with
    | [] -> acc
    | x::t -> rev' t (x::acc)
  in rev' l [];; 

let rev_app l1 l2 = 
  let rec rev_app' l1 l2 = match l1 with
    | [] -> l2
    | x::t -> rev_app' t (x::l2)
  in rev_app' (rev l1) l2;;

let nth l n = if n > length l || n < 0 then failwith "Bad index" else
    let rec atIndex' l n= match l, n with 
      | [], _ -> failwith "Bad index"
      | x::t, 0 -> x
      | x::t, _ -> atIndex' t (n-1)
    in atIndex' l n;;

(* 1. In python main, function backtrack called with 5 initial argument: 
   sequence: int list | path: 'a list | answer: 'a list | target: int | index: int
   basically: call to "in backtrack [...] [] [] n 0" after definition.
   => answer argument is actually redudant for OCaml implementation & can be computed inplace
*)

let backtrack cands path target prev_idx =
  let rec backtrack' cands path target prev_idx =
    if target = 0 then [rev path]
    else let rec loop idx = match (length cands - idx) with
        | 0 -> []
        | _ -> let curr = nth cands idx in 
            if target >= curr then
              let res = backtrack' cands (path @ [curr]) (target-curr) idx in
              rev_app res (loop (idx+1))
            else loop (idx+1)
      in loop prev_idx
  in backtrack' cands path target prev_idx 
    
let combination_sum l n = backtrack l [] n 0;;

let test1 = (combination_sum [2;3;5] 8) = [[2;2;2;2]; [3;3;2]; [5;3]];;
let test2 = (combination_sum [2;3;6;7] 7);;
