open Gamebase
open Game
open Functory.Network
open Functory.Network.Same

(* Interactively ask for the player's move. 
 * Returns Some move, or None when the move is invalid. *)
let ask_move state =
  Printf.printf "  => Your move ? (should look like (stack,nbofmatchstick)) %!" ;  
  let line = read_line () in

  match readmove line with
  | None ->
    Printf.printf "\n Cannot read this move: %s\n\n%!" line ;
    None
    
  | Some mov ->
    if not (is_valid state mov) then
      begin
        Printf.printf "\n This move is invalid: %s\n\n%!" (move2s mov) ;
        None
      end
    else Some mov

(* Get the move from the IA.
*Uncomment if needed*)
(*let ia_move state =
  let (mov, _) = Game_ia.best_move state in
    match mov with
    | None -> assert false
    | Some m -> m*)

(*Get the move from the IA using parallelized version*)
let ia_move state =
  let mov = Game_ia.best_move_parallelized state in
    match mov with
    | None -> assert false
    | Some m -> m
  
(*** Each player in turn. ***)
    
let rec run with_ia state =
  
  (* Print state & which player to play. *)
  Printf.printf "\n%s\n %s to play.\n\n%!" (state2s state) (player2s (turn state)) ;
  
  match result state with
  | Some r ->
    (* Game is finished. Print result. *)
    Printf.printf "*** %s ***\n%!" (result2s r) ;
    ()
    
  | None ->
    (* Game is not finished. Play one turn. *)

    let state' =
      if with_ia && turn state = Comput
      then play state (ia_move state)
      else
        begin match ask_move state with
          | None -> state (* Invalid move, play same state again. *)
          | Some mov -> play state mov
        end
    in
    run with_ia state'

(*Launch the ssh connexion and the program to be set as a worker*)
let set_workers hosts nbworker =
  let start_worker =
    fun host nw -> let ssh = "ssh " ^ host ^ " " ^ Sys.executable_name ^ " worker " in 
    ignore(Sys.command ("xterm -hold -e '" ^ ssh ^ "' &"));
    declare_workers  ~n:nw host;
  in 
  List.iter2 start_worker hosts nbworker;;

(*let () = run true initial (false : pas d'IA)*)

(*** Entry point of the program. ***)
let () =
  (* Sys.argv are the command-line arguments. *)
  match Sys.argv with
  (* If there is one argument equal to "master" *)
  | [| _ ; "master" |] -> 
     Printf.printf "I am the master.\n%!" ;
     declare_workers ~n:2 "localhost" ;
     run true initial;
  (* Otherwise, we are a worker. *)
  | _ -> 
     Printf.printf "I am a worker.\n%!" ;
     Functory.Network.Same.Worker.compute ()

(* Tried entry point of the program with ssh connexion *)
(*let () =
  (* Sys.argv are the command-line arguments. *)
  match Sys.argv with
  (* If there is one argument equal to "master" *)
  | [| _ ; "master" |] -> 
     Printf.printf "I am the master.\n%!" ;
     set_workers ["localhost"] [2];
     run true initial;

  (* Otherwise, we are a worker. *)
  | _ -> 
     Printf.printf "I am a worker.\n%!" ;
     Functory.Network.Same.Worker.compute ()*)

