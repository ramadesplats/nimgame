open Gamebase

(* These types are abstract in game.mli *)

type state = int * player

type move = int

type result = Win of player

(* Printers *)
let state2s (n,p) = Printf.sprintf "Current = %d  // %s to play" n (player2s p)

let move2s n = Printf.sprintf " +%d" n

let result2s (Win p) = (player2s p) ^ " wins"

(* Reader *)
let readmove s = try Some (int_of_string s) with _ -> None

(* You have to provide these. *)
let initial = (0,Human);;

let turn = function 
	| (_,b) -> b;;

(* return true is the move is valid *)
let is_valid s m = 
	match s with 
	| (a,_)->(a+m)<=20 && m<=3;;

let play s m = 
	match s with 
	| (a,b)-> if is_valid s m then (a+m,Gamebase.next b) else failwith "Move not playable";;

let all_moves s = [1;2;3];;

let result s =
	match s with 
	|(a,b)-> if a=20 then Some(Win(b)) else None;;

(* This type was given in game.mli.
 * We have to repeat it here. *)
type comparison = Equal | Greater | Smaller	

(*
	match p with
	| Human -> begin match r1,r2 with
		| Win Human ,Win Human ->Equal
		| Win Human ,Win Comput ->Smaller
		| Win Comput ,Win Human ->Greater end
	| Comput -> begin match r1,r2 with 
		| Win Human ,Win Human ->Equal
		| Win Comput ,Win Comput ->Equal
		| Win Human,Win Comput->Greater
		| Win Comput,Win Human->Smaller end;;
*)

let compare p r1 r2 =
	match p,r1,r2 with 
	| Human, Win Human, Win Comput | Comput, Win Comput, Win Human -> Smaller
	| Human, Win Comput, Win Human | Comput, Win Human, Win Comput -> Greater
	| _,_,_-> Equal;;


let worst_for p = 
	match p with 
	|Human -> Win Comput
	|Comput -> Win Human;;
  
