open Game
open Gamebase

(* Returns the best result the current player can reach. 
 * Returns None if the game is finished in the current state. *)
val best_move: state -> move option * result 

(*Returns the best result of the player using the hashtable
Faster thand only best_move*)
val best_move_cache: state -> move option * result 

(*Returns the best result using zemap(using bestmove and calculating the result of this move)
* and zefold that select the best result*)
val best_move_parallelized: state -> move option
