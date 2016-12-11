open Game
open Gamebase

let memory = Hashtbl.create 100000;;

(* Stupid IA: it take the first possible valid move.
let best_move state =
  match List.filter (is_valid state) (all_moves state) with
  | [] -> assert false
  | m :: _ ->
    let player = turn state in
    (Some m, worst_for player);; *)



(*let rec best_move state =
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
      (Some x,y);;*)


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


let rec best_move state =
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
          | None -> aux s t ((Some h, snd (best_move s2))::moves)
    in
    match r with
      | Some res ->
        let woup = (None, res) in
        Hashtbl.add memory state woup;
        woup
      | None -> 
        let woup = aux state (List.filter (fun a -> is_valid state a) lm) [] in
        Hashtbl.add memory state woup;
        woup
;;
