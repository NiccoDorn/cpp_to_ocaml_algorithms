(* 
   This one is actually pretty easy because all combinations a sequence with size k := [1,n]
   is a subset of all possible subsequences in the initial sequence a.k.a. all sublists.  
   One could potentially split "all subsequences" into three groups: 
   1. all prefix sequences 2. all suffix sequences 3. all (Sub-)Intervals. They overlap.

   Runnning Time complexity: O(2^n), which resembles the amount of computed subsequences in n, 
   where n is the length of the initial sequence.
*)

let length l = 
  let rec len l acc = match l with
    | [] -> acc
    | x::t -> len t (acc+1)
  in len l 0;;

let create n = if n < 1 then [] else
    let rec createl' c acc = match (n-c) with
      | 0 -> acc @ [c]
      | _ -> createl' (c+1) (acc @ [c])
    in createl' 1 [];;

let sublists lst =
  let rec subl' acc1 acc2 = match acc2 with
    | [] -> acc1
    | x::xs -> 
        let rec prepend_all acc acc1' = match acc1' with
          | [] -> acc
          | sub::rest -> prepend_all ((x::sub)::acc) rest
        in subl' (prepend_all acc1 acc1) xs
  in subl' [[]] lst;; 

(* Num of subsequences is 2^n, n = lenght of initial sequence *)
let test1 = length (sublists (create 4)) = 16;;
let test2 = length (sublists (create 5)) = 32;;
let subseq5 = sublists (create 5);;

