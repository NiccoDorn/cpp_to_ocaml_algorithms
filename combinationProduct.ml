(* This problem is  storngly related to the combination sum problem
   Thus, it suffices to have some adjustments in the combinationSum backtrack function.
   1. Base Case needs to be adjusted to 1 instead of 0 in the pattern match
   2. On recursive backtrack' call, the target needs to be update by div instead of sub
   3. The check in the second match case needs to check for target mod curr = 0
*)

let backtrack cands path target prev_idx =
  let rec backtrack' cands path target prev_idx =
    if target = 0 then [rev path]
    else let rec loop idx = match (length cands - idx) with
        | 1 -> []
        | _ -> let curr = nth cands idx in 
            if target mod curr = 0 then
              let res = backtrack' cands (path @ [curr]) (target/curr) idx in
              rev_app res (loop (idx+1))
            else loop (idx+1)
      in loop prev_idx
  in backtrack' cands path target prev_idx 
    
let combination_prod l n = backtrack l [] n 0;;

let test1 = (combination_prod [2;3;5] 8) = [[2;2;2;2]; [3;3;2]; [5;3]];;
let test2 = (combination_prod [2;3;6;7] 18);;
