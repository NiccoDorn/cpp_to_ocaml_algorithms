(* All combinations of [1,n] in k-sized lists *)

(* type scheme: int -> int -> int list list *) 
(* approach: create list of elements from n, thenn compute all sublists
   and finally filter only for lists with k elements *)
let  len l = 
  let rec len' l c = match l with
    | [] -> c
    | h::t -> len' t (c+1)
  in len' l 0;; 

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

let allCombs n k = if k > n || n < 0 || k < 0 then [[]] else
    let l = sublists (create n) in
    let rec getk l acc = match l with
      | [] -> acc
      | x::t -> if len x = k then getk t (x::acc) else getk t acc
    in getk l [];;

allCombs 5 2;; 

(* Testing: for pairs the combination result length follow the gaussian formula *)
let test n = (n * (n-1)) / 2 = len (allCombs n 2);;
test 5;;

allCombs (-1) 1 = [[]];; (* function argument tests *)
allCombs 1 (-1) = [[]];;
allCombs 1 2 = [[]];;
allCombs 1 1 = [[1]];; 
len (allCombs 4 4) = 1;; (*If n = k => amount elements is always 1 *) 
len (allCombs 7 7);;

(* Given an arbitrary n & k with validitiy restrictions,
the number of computed combinations will be n over k. *)
let test = len(allCombs 6 3) = 20;;
(* 6*5*4 / 3*2*1 = 120 / 6 = 20 => Correct! *)
(* for k = 1 to k = n, the total amount of computed sublists for COMBINATIONS
(not permutations!) will be 2^n *)
let countCombs n = if n < 0 then 0 else
    let rec countCombs' k amount = match (n-k) with
      | -1 -> amount
      | _ -> countCombs' (k+1) (amount + len(allCombs n k))
    in countCombs' 0 0;;
let test = (countCombs 5) = 32;; (* 2^5 = 32 => Correct! *)
