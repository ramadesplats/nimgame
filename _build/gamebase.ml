type player = Human | Comput

(* Next turn *)
let next = function
  | Human -> Comput
  | Comput -> Human

let player2s = function
  | Human -> "Human"
  | Comput -> "Comput"



(* Helper functions on matrices *)

type 'a matrix = 'a array array

let create_matrix n m vale =
  let result = Array.make n (Array.make m vale) in
    for i = 1 to n - 1 do
      result.(i) <- Array.make m vale
    done;
    result;;

let count_ones row =
  Array.fold_left (fun c x -> if x=1 then c+1 else c) 0 row;;

let clean m l num =
  let n=(count_ones m.(l))-num in 
  (*Printf.printf "l = %d num=%d\n%!" l num;
  Printf.printf "for i = %d to %d\n%!" n (Array.length m.(l) -1) ; *)
    for i=n to Array.length m.(l) -1 do
      m.(l).(i) <- 0
    done; 
    m;;

(*let count m i =
  let ret=Array.fold_left (fun x y -> if y=1 then x+1 else x) 0 (m.(i)) in 
  ret;;*)

let countall mat k =
  Array.mapi (fun i row -> if i = k then count_ones row else count_ones row) mat
  |> Array.fold_left (+) 0;;

let clone_matrix m =
  let cloned = Array.copy m in
  Array.iteri (fun i line -> cloned.(i) <- Array.copy line) cloned ;
  cloned

exception Found of int * int

let find_cell m p =
  try
    for row = 0 to Array.length m - 1 do
      let line = m.(row) in
      for col = 0 to Array.length line - 1 do
        if p line.(col) then raise (Found (row, col))
      done ;
    done;
    None
    
  with Found (r,c) -> Some (r,c)

let line2s v2s line = Array.fold_left (fun s v -> s ^ v2s v ^ "|") "|" line

let linesep = "-------\n"

(* Transforms a grid to a string. *)
let matrix2s m v2s =
  if Array.length m = 0 then "(empty matrix)"
  else
    let firstline = line2s v2s m.(0) in
    let linesep = (String.make (String.length firstline) '-') ^ "\n" in
    Array.fold_left (fun s line -> s ^ (line2s v2s line) ^ "\n" ^ linesep) linesep m
