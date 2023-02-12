open AST

(*
0 = nothing
1 = wall
2 = redwall   
*)

let tile_of_char c =     
    match c with
    | '0' -> NOTHING
    | '1' -> WALL
    | '2' -> RED_WALL
    | '3' -> TRANSPARENT_WALL
    | _ -> failwith "unidenfied char";;

let parsePlot path = 
    let ic = open_in ("main/resources/" ^ path) in
    let player_pos_str = input_line ic in
    let l = String.split_on_char ';' player_pos_str in
    if List.length l <> 2 then failwith (
        Printf.sprintf "invalid player pos '%s' %d" player_pos_str (List.length l));
    let player_pos = {
        x = (float_of_string (List.nth l 0));
        y = (float_of_string (List.nth l 1));
    } in

    let lines = ref [] in
    let line = ref [] in
    let lines = (try
        while true do 
            let c = input_char ic in
            if c = '\n' then (
                lines := (List.rev !line) :: !lines;
                line := []
            ) else if c <> ';' then
                line := c :: !line
        done; !lines
      with End_of_file ->
        close_in ic;
        List.rev (!lines)
    ) in


    let arrays = Array.init (List.length lines) (fun _ -> [||]) in
    List.iteri
        (fun i line -> 
            let array = Array.init (List.length line) (fun _ -> NOTHING) in
            List.iteri 
                (fun j c -> if c <> ';' then array.(j) <- tile_of_char c )
                line;
            arrays.(i) <- array
        )
        lines;
    player_pos, arrays
    ;;
