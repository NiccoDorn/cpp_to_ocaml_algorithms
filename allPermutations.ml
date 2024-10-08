let rec length l = match l with
  | [] -> 0
  | _::t -> 1 + length t;;

let rec createpos n i acc = match (n-i) with
  | 0 -> (acc @ [i])
  | _ -> createpos n (i+1) (acc @ [i]);;
let rec createneg n i acc = match (n-i) with
  | 0 -> (acc @ [i])
  | _ -> createneg n (i-1) (acc @ [i]);; 

let create n = if n = 0 then [] else if n > 0 
  then createpos n 1 [] else createneg n (-1) [];; 

let rec append l1 l2 = match l1 with
  | [] -> l2
  | x::xs -> x :: append xs l2;;

let rec remove i l = 
  match l with
  | [] -> []
  | x :: xs -> if i = 0 then xs else x :: remove (i - 1) xs;;

let rec accumulate acc f lst = 
  match lst with
  | [] -> acc
  | x::xs -> accumulate (f acc x) f xs;;

let rec permutations lst =
  match lst with 
  | [] -> []                         (* Basically: Go from larger list into   *)
  | [x] -> [[x]]                     (* smaller ones recursively & from       *) 
  | _ ->                             (* 1-elem sublists, back to n-len list   *)
      let rec loop i acc = 
        if i = length lst then acc
        else
          let m = List.nth lst i in
          let remLst = remove i lst in
          let perms = permutations remLst in
          let updated_acc = accumulate acc (fun acc p -> append acc [[m] @ p]) perms in
          loop (i + 1) updated_acc
      in loop 0 [] ;;

let test1 = length (permutations (create 5)) = 120;;
let test2 = length (permutations (create (-6))) = 720;;

(* 
   Explanation: For every current number of a sublist, compute its (n-1)!
   permutations recursively. For example: In a list of length 5, every number has 
   (5-1)! permutated sublist length, the list is built bottom-up by computing smaller
   sublists, performing swap operations for it, and building that list.
*)
