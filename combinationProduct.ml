(* This problem is strongly related to the combination sum problem
   Thus, it suffices to have some adjustments in the combinationSum backtrack function.
   1. The base case for variable target changes from 0 to 1 (l.10)
   2. The check in the second match case needs to check for target mod curr = 0 (l.14)
   3. On recursive backtrack' call, the target needs to be update by div instead of sub (l.15)

   The utility functions from combinationSum are used here.
*)

let backtrack cands target = if target = 1 
  then if exists cands 1 then [[1]] else [[]] 
  else let rec backtrack' cands path target prev_idx =
         if target = 1 then [rev path]
         else let rec loop idx = match (length cands - idx) with
             | 0 -> []
             | _ -> let curr = nth cands idx in 
                 if target mod curr = 0 then
                   let res = backtrack' cands (path @ [curr]) (target/curr) idx in
                   rev_app res (loop (idx+1))
                 else loop (idx+1)
           in loop prev_idx
    in backtrack' cands [] target 0;; 
    
let combination_prod l n = backtrack l n;; 
let test1 = (combination_prod [2;3;4] 8) = [[2;2;2]; [4;2]];;
let test2 = combination_prod [2;3;6;9] 18;;
let test3 = combination_prod [2;3;4] 1;;
let test4 = combination_prod [1;3;4] 1;;
