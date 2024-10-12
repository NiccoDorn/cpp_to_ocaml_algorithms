(* This problem is strongly related to the combination sum problem
   Thus, it suffices to have some adjustments in the combinationSum backtrack function.
   1. The base case for variable target changes from 0 to 1 (l.10)
   2. The check in the second match case needs to check for target mod curr = 0 (l.14)
   3. On recursive backtrack' call, the target needs to be update by div instead of sub (l.15)
   4. Adding functions to realize different combinations of 1-sequences for target=1.

   The utility functions from combinationSum are used here.
*)

let rec count l n =
  let rec count' l n a = match l with
    | [] -> a
    | x::t -> if x = n then count' t n (a+1) else count' t n a
  in count' l n 0;; 

let add_ones l = 
  let rec add_ones' l acc = match l with
    | [] -> acc
    | x::t -> add_ones' t ((1::x)::acc)
  in rev (add_ones' l []);;

let ones n =
  let rec ones' n acc = match n with
    | 0 -> acc
    | _ -> ones' (n-1) ([1]::(add_ones acc))
  in ones' n [];;

let backtrack cands target = if target = 1 
  then if (count cands 1) > 0 then ones (count cands 1) else [[]] 
  else let rec backtrack' cands path target prev_idx =
         if target = 1 then [rev path]
         else let rec loop idx = match (length cands - idx) with
             | 0 -> []
             | _ -> let curr = nth cands idx in 
                 if target mod curr = 0 && curr != 1 then
                   let res = backtrack' cands (path @ [curr]) (target/curr) idx in
                   rev_app res (loop (idx+1))
                 else loop (idx+1)
           in loop prev_idx
    in backtrack' cands [] target 0;;

let combination_prod l n =  if (count l 1) > 0 then add_ones (backtrack l n)
  else backtrack l n;;

let test1 = (combination_prod [2;3;4] 8) = [[2;2;2]; [4;2]];;
let test2 = combination_prod [2;3;6;9] 18;;
let test3 = combination_prod [2;3;4] 1;;
let test4 = combination_prod [1;3;4] 4;;
