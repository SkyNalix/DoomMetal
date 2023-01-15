open AST
open Printf


(* Define a function to perform the raycast *)
let raycast level mouse_pos =
    let plot = level.plot in
    let player_pos = level.player.pos in

    let pos = {x=player_pos.x; y=player_pos.y} in

    let rayDir = (
      let vec_x = mouse_pos.x -. player_pos.x in
      let vec_y = mouse_pos.y -. player_pos.y in
      let vec_length = sqrt (vec_x *. vec_x +. vec_y *. vec_y) in
      {
        x=(vec_x /. vec_length); y=(vec_y /. vec_length)
      }
    ) in

    let map = {x=float_of_int (int_of_float pos.x); y= float_of_int (int_of_float pos.y)} in
    let sideDist = {x=0.; y=0.} in
    let deltaDist = {
        x= if rayDir.x = 0. then 999. else Float.abs (1. /. rayDir.x);
        y= if rayDir.y = 0. then 999. else Float.abs (1. /. rayDir.y);
    } in
    let step = {x=0.; y=0.} in
    let hit = ref false in
    let side = ref 0 in

    if rayDir.x < 0. then (
        step.x <- (-1.);
        sideDist.x <- (pos.x -. map.x) *. deltaDist.x;
    ) else (
        step.x <- 1.;
        sideDist.x <- (map.x +. 1. -. pos.x) *. deltaDist.x;
    );
    if rayDir.y < 0. then (
        step.y <- (-1.);
        sideDist.y <- (pos.y -. map.y) *. deltaDist.y;
    ) else (
        step.y <- 1.;
        sideDist.y <- (map.y +. 1. -. pos.y) *. deltaDist.y;
    );

    while not (!hit) do (
        if sideDist.x < sideDist.y then (
            sideDist.x <- sideDist.x +. deltaDist.x;
            map.x <- map.x +. step.x;
            side := 0;
        ) else (
            sideDist.y <- sideDist.y +. deltaDist.y;
            map.y <- map.y +. step.y;
            side := 1;
        );
        if (plot.(int_of_float map.x).(int_of_float map.y) <> NOTHING) then hit := true;
    ) done;

    let perpWallDist = (
        if !side == 0 then (sideDist.x -. deltaDist.x)
        else (sideDist.y -. deltaDist.y);
    ) in

    (!hit, perpWallDist, map)
