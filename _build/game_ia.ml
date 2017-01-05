open Game
open Gamebase

open Functory.Network
open Functory.Network.Same

let memory = Hashtbl.create 100000;;

(*convert a line into a string*)
let rec line2s= function 
  | [] -> Printf.printf "\n%!"
  | hd::tl -> let (a,b)=hd in 
        match a with 
          | Some (c) -> Printf.printf " %s - " (move2s c); line2s tl
          | None -> Printf.printf " nothing "; line2s tl

(* Stupid IA: it take the first possible valid move.
let best_move state =
  match List.filter (is_valid state) (all_moves state) with
  | [] -> assert false
  | m :: _ ->
    let player = turn state in
    (Some m, worst_for player);; *)

(*First version of best_move*)
let rec best_move state =
  match result state with  
    |Some x -> (None,x)
    |_ -> let l = List.filter (is_valid state) (all_moves state) in 
      let rec loop l best_m best_r =
        match l with
          |[] -> (best_m,best_r) 
          |a::b-> let player = turn state in
              let (_,d) = best_move (play state a) in 
                match (compare player (best_r) (d)) with
                  |Smaller-> loop b best_m best_r
                  |Equal -> loop b a best_r
                  |_ ->  (a,d)
      in let (x,y)= loop l (List.hd l) (worst_for (turn state))in
      (Some x,y);;


(*fin the best move and the best result in a list*)
let find_max p l =
  (*mm=max move et mr = max result*)
  let rec aux p l (mm,mr) =
    match l with
      | [] -> (mm,mr)
      | ((None,_)::t) -> failwith "Erreur"
      | ((Some m,r)::t) -> match (compare p mr r) with
        | Greater -> aux p t (Some m,r)
        | Equal -> aux p t (Some m,r)
        | Smaller -> aux p t (mm,mr)
  in
  aux p l (List.hd l)
;;

(*best_move version using a cache*)
let rec best_move_cache state =
  if Hashtbl.mem memory state then Hashtbl.find memory state
  else
    let lm = all_moves state (* les mouvements possibles *)
    and p = turn state  (* le joueur courant *)
    and r = result state  (* Soit on a gagne soit rien *)
    in
    let rec aux s l moves =
      match l with
       | [] -> find_max p moves
       | h::t -> 
        let s2 = play s h
        in
        match result s2 with
          | Some res -> aux s t ((Some h, res)::moves)
          | None -> aux s t ((Some h, snd (best_move_cache s2))::moves)
    in
    match r with
      | Some res ->
        let woup = (None, res) in
        Hashtbl.add memory state woup;
        woup
      | None -> 
        let woup = aux state (List.filter (fun a -> is_valid state a) lm) [] in
        Hashtbl.add memory state woup;
        woup;;

let rec foldb acc x =
  match x with 
    |[]->acc
    |[res]->res::acc
    |a::b->a::foldb acc b;;


(*lighter version of find max for parallelized version of best_move*)
let bestresult player a b = 
  match compare player (snd a) (snd b) with
  | Greater -> b
  | _ -> a;;

(*best_move version using the functory module*)
let best_move_parallelized state = 
  match result state with
  | Some(x) -> None
  | None ->
    let moves = List.filter (is_valid state) (all_moves state) in
    let zemap = fun move -> Some(move, snd (best_move (play state move))) in
    let zefold = fun a b -> match (a, b) with
      | None, _ -> b
      | _, None -> a
      | Some a_, Some b_ -> Some(bestresult (turn state) a_ b_)
    in
    match map_fold_ac ~f:zemap ~fold:zefold None moves with
      | None -> None
      | Some(move, result) -> Printf.printf "######SOLUTION-FOUND######\n%!" ;
      Unix.select [] [] [] 2.0  ; Some(move)


(*map : ('a -> 'b) -> 'a list -> 'b list

let zefold = fun a b -> match (a, b) with
      | None, _ -> b
      | _, None -> a
      | Some a_, Some b_ -> Some(bestresult (turn state) a_ b_)

let rec aux = function 
    | [] -> []
    | mv :: tl -> (play state mv)::(aux tl)           
  in 

fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    --> List.fold_left f a [b1; ...; bn] is f (... (f (f a b1) b2) ...) bn.

map_fold_ac : f:('a -> 'b) -> fold:('b -> 'b -> 'b) -> 'b -> 'a list -> 'b*)


