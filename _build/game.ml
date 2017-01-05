open Gamebase

(* These types are abstract in game.mli *)

type state = int matrix * player

type move = int * int

type result = Win of player

(* Printers *)
let state2s (n,p) = Printf.sprintf "Current : \n%s  // %s to play" (matrix2s n string_of_int) (player2s p)

let move2s (n,m) = Printf.sprintf " Taking in %dth stack : %d matchstick" (n+1) (m)

let result2s (Win p) = (player2s p) ^ " wins"

(* Reader *)

let readmove s =
  try Some (Scanf.sscanf s "(%d,%d)" (fun a b -> (a-1,b)))
  with _ -> None

(* Create a game with random number of matchstick for each stack
* Not always the same player that start the game
* let a = create_matrix 5 10 1 in
*)
let initial =
	let a = create_matrix 2 10 1 in 
	(* use only 2 stacks for parallelized demonstration purpose*)
		Random.self_init ();
		for i=0 to 1 do 
			let rand = Random.int 10 in
			a=clean a i rand;
		done; 	
	let b = Random.int 10 in 
		if b>= 5 then (a,Human) else (a,Comput);;

(*return the player playing the turn*)
let turn = function 
	| (_,b) -> b;;

(* return true is the move is valid *)
let is_valid (a,_) (b,c) =
	Printf.printf "a : %s b : %d c : %d%!\n" (matrix2s a string_of_int) b c;
	b<=4 && b>=0 && c<=10 && c>0 &&
	let count=Array.fold_left (fun x y -> if y=1 then x+1 else x) 0 (a.(b)) in
	(*Printf.printf "count : %d%!\n" count;*)
	count >= c;;

(*state,move*)
(*"play" actually play a turn so we have to clone the matrix to avoid side effects*)
let play s m =
	match s with 
		|(a,d)-> (match m with
			|(b,c)-> if is_valid s m then let cloned =
			clone_matrix a in 
				let res = clean cloned b c in 
					(res,next d)
					else failwith "Move not playable");;

(*reverse a list*)
let rec rev l =
	let rec aux acc = function
		|[]->acc
		|a::b->aux (a::acc) b
	in aux [] l;; 

(*auxiliar function creating a list of int between 1 and the max of matchstick per line*)
let separated_moves (a,b)=
	let rec aux count acc = 
		match count with
			|0->acc
			|_->aux (count-1) ((a,count)::acc) in 
	aux b [];;

(*append all the moves in a list*)
let array_move l =
	let rec aux acc = function
		|[]->acc
		|(a,b)::c->aux (List.append acc (separated_moves(a,b)) ) c in 
	aux [] l;;

(*auxiliar function counting how many matchsticks are in a matrix per line*)
let count_array mat =
	Array.mapi (fun i row -> (i, count_ones row)) mat
	|> Array.to_list |> List.filter (fun (_, c) -> c != 0);;

(*return all the moves that can be played*)
let all_moves (mat, _) =
	array_move (count_array mat);;

(*return Some(Winner) if the game has been won else None*)
let result (mat,player)=
	if (countall mat 0)=0 then Some(Win(next(player))) else None;;

(* This type was given in game.mli.
 * We have to repeat it here. *)
type comparison = Equal | Greater | Smaller	

(*compare two results for a player*)
let compare p r1 r2 =
	match p,r1,r2 with 
	| Human, Win Human, Win Comput | Comput, Win Comput, Win Human -> Smaller
	| Human, Win Comput, Win Human | Comput, Win Human, Win Comput -> Greater
	| _,_,_-> Equal;;

(*return the other state for a player*)
let other_player p =
	match p with 
	|Human -> Comput
	|Comput -> Human;;

(*return the worst state for a player*)
let worst_for p = 
	match p with 
	|Human -> Win Comput
	|Comput -> Win Human;;

(*Comments for a later understanding*)

(*let all_moves (mat, _) =
  Array.mapi (fun i row -> (i, count_ones row)) mat
  |> Array.to_list |> List.filter (fun (_, c) -> c != 0)*)
  
