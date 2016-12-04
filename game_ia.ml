open Game

(* Stupid IA: it take the first possible valid move.
let best_move state =
  match List.filter (is_valid state) (all_moves state) with
  | [] -> assert false
  | m :: _ ->
    let player = turn state in
    (Some m, worst_for player);; *)



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