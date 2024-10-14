(*
      O  O  O  O
      |  |  |  |
   O--O--O--O--O--O
      |  |  |  |
      O--O--O--O      : Example Graph
      |  |  |  |
   O--O--O--O--O--O
      |  |  |  |
      O  O  O  O
      
      => With 3 Colors, e.g. RGB (3-Coloring):

      R  G  B  R
      |  |  |  |
   R--G--B--R--G--B
      |  |  |  |
      R--G--B--R      => 3-Colored Graph: adjadent vertices have different color.
      |  |  |  |
   R--G--B--R--G--B
      |  |  |  |
      R  G  B  G
      
  Coloring Problem in general consists of 2 elements:
  1. Checking, if a graph coloring is valid (Decision Problem)
  2. The coloring-process itself.
      
*)

(* Utility functions *)
let length l = 
  let rec len l acc = match l with
    | [] -> acc
    | x::t -> len t (acc+1)
  in len l 0;; 

let rec exists f l1 l2 = match (l1, l2) with
  | ([], _) | (_, []) -> false
  | (h1 :: t1, h2 :: t2) -> f h1 h2 || exists f t1 t2;;

let rec nth lst n = match lst with
  | [] -> failwith "Bad index!"
  | h :: t -> if n = 0 then h else nth t (n - 1);;

(* Code for coloring & color-checking will follow *)
