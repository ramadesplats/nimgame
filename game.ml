open Gamebase

(* These types are abstract in game.mli *)

type state = int matrix * player

type move = int * int

type result = Win of player

(* Printers *)
let state2s (n,p) = Printf.sprintf "Current : \n%s  // %s to play" (matrix2s n string_of_int) (player2s p)

let move2s (n,m) = Printf.sprintf " Taking %d matchstick from %d stack" n m

let result2s (Win p) = (player2s p) ^ " wins"

(* Reader *)

let readmove s =
  try Some (Scanf.sscanf s "(%d,%d)" (fun a b -> (a,b)))
  with _ -> None

(* Create a game with random number of matchstick for each stack
* Not always the same player that start the game
*)
let initial =
	let a = ref (create_matrix 5 10 1) in 
	Printf.printf "%s%!" (matrix2s !a string_of_int);
		for i=0 to 4 do 
			let rand = Random.int 10 in
			Printf.printf "rand : %d%!" rand;
			a:=clean !a i rand;
			Printf.printf "%s%!" (matrix2s !a string_of_int);
		done; 
	let b = Random.int 10 in
		if b>= 5 then (!a,Human) else (!a,Comput);;

let turn = function 
	| (_,b) -> b;;

(* return true is the move is valid *)
let is_valid s m =
	match s with 
		|(a,_)->match m with 
		|(b,c)-> let count=Array.fold_left (fun x y -> if y=1 then x+1 else x) 0 (a.(b)) in 
		count>=c;;

(*state,move*)
let play s m =
	match s with 
		|(a,d)-> match m with 
		|(b,c)-> if is_valid s m then let cloned =
		clone_matrix a in 
		let res = clean cloned b c in 
				(res,next d)
			else failwith "Move not playable";;

(* cloner la matrice pour pas modifier une matrice d'un état existant. En effet play joue un coup*)

(*let all_moves s =
	match s with 
	|(a,_)->Gamebase.clear[(0,Gamebase.count a 0);(1,Gamebase.count a 1);(2,Gamebase.count a 2);(3,Gamebase.count a 3);(4,Gamebase.count a 4)];;*)

let all_moves (mat, _) =
  Array.mapi (fun i row -> (i, count_ones row)) mat
  |> Array.to_list |> List.filter (fun (_, c) -> c != 0)

let result (mat,player)=
	if (countall mat 0)=0 then Some(Win(player)) else None;;

(* This type was given in game.mli.
 * We have to repeat it here. *)
type comparison = Equal | Greater | Smaller	

let compare p r1 r2 =
	match p,r1,r2 with 
	| Human, Win Human, Win Comput | Comput, Win Comput, Win Human -> Smaller
	| Human, Win Comput, Win Human | Comput, Win Human, Win Comput -> Greater
	| _,_,_-> Equal;;


let worst_for p = 
	match p with 
	|Human -> Win Comput
	|Comput -> Win Human;;
  
