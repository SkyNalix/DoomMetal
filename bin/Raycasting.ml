open AST
open Bogue
module W = Widget
module A = Sdl_area

(* Define a function to perform the raycast *)
let raycast_on_angle level mouse_pos =
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

    let rec aux (hit:bool) side = (
        if hit then hit, side else
        let side = (
            if sideDist.x < sideDist.y then (
            sideDist.x <- sideDist.x +. deltaDist.x;
            map.x <- map.x +. step.x;
            0
            ) else (
                sideDist.y <- sideDist.y +. deltaDist.y;
                map.y <- map.y +. step.y;
             1
            )
        ) in
        let hit = plot.(int_of_float map.x).(int_of_float map.y) <> NOTHING in
        aux hit side;
    ) in
    let (hit, side) = aux false 0 in

    let perpWallDist = (
        if side == 0 then (sideDist.x -. deltaDist.x)
        else (sideDist.y -. deltaDist.y);
    ) in

    (hit, perpWallDist, map);;


let aux_raycast windows_info level angle angle_min angle_max =

    let width= windows_info.width in
    let height = windows_info.height in
    let player = level.player in
    let block_width = windows_info.block_width in
    let block_height = windows_info.block_height in

    let radians = (float_of_int angle) *. (Float.pi /. 180.) in
    let viewVect = { x=sin radians; y=cos radians} in
    viewVect.x <- (viewVect.x *. (float_of_int width)  +. (float_of_int block_width *. player.pos.x ));
    viewVect.y <- (viewVect.y *. (float_of_int height) +. (float_of_int block_height *. player.pos.y ));

    (* positions de la souris relativement dans le plot *)
    let mouse_pos = {x=viewVect.x /. float_of_int block_width; y=viewVect.y /. float_of_int block_height} in
    let (a,b,c) = raycast_on_angle level mouse_pos in
    {
        rayTouched = a;
        distance = b; 
        touched_pos = c;
        angle = angle;
        angle_vec = viewVect;
        angle_min = angle_min;
        angle_max = angle_max;
    };;



let rec raycast_rec windows_info level (angle_min:int) (angle_max:int) (step:int) cur_angle =
    let ray = aux_raycast windows_info level cur_angle angle_min angle_max in
    if cur_angle = angle_max then [ray] else
    let cur_angle = min angle_max (cur_angle+step) in
    ray :: (raycast_rec windows_info level angle_min angle_max step cur_angle)


let raycast windows_info level = 
    let angle = level.player.view_angle in
    let rays = raycast_rec windows_info level (angle-25) (angle+25) 1 (angle-25) in
    List.iteri (fun i ray -> Drawer3D.drawRay windows_info level ray i) rays;
    Drawer2D.drawLevel windows_info level;
    List.iter (fun ray -> Drawer2D.drawRay windows_info level ray) rays;
    ();;