(* Bit Manipulation: Count Bit Flips *) 
let bitseq_a = 149857;;
let bitseq_b = 203495;;

let countBitFlips a b =
  let a' = a lxor b in
  let rec cnt a' c = 
    if a' <= 0 then c else cnt (a' land (a' - 1)) (c+1)
  in cnt a' 0;;
  
let numBitFlips = countBitFlips bitseq_a bitseq_b;;
Printf.printf "%d\n" numBitFlips;;

(* Checking both bit sequences with the land 1 operation to get LSB *)
let checkDiffBit a b = if (a land 1) = (b land 1) then 0 else 1;;
let countDiffBits a b =
  let rec cnt' a b c = match a, b with
    | 0, 0 -> c
    | 0, b -> cnt' a (b lsr 1) (c + checkDiffBit a b)
    | a, 0 -> cnt' (a lsr 1) b (c + checkDiffBit a b)
    | a, b -> cnt' (a lsr 1) (b lsr 1) (c + checkDiffBit a b)
  in cnt' a b 0;;

let numDiffBits = countDiffBits 149857 203495;;

(* Testing *)
let test_true = (countBitFlips 149857 203495) = (countDiffBits 149857 203495);;
let test_false = (countBitFlips 149857 203495) = (countDiffBits 149857 203494);;

let al = 0b1100000000000000000000001;; 
let bl = 0b0000000000000000000000001;;

let test = (countBitFlips al bl) = 2 && (countDiffBits al bl) = 2;;
