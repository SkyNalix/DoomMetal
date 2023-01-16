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


let aux_raycast windows_info level angle =

    let width= windows_info.width in
    let height = windows_info.height in
    let player = level.player in
    let block_width = (width / (Array.length level.plot.(0))) in
    let block_height = (height / (Array.length level.plot)) in

    let player_posX = int_of_float (float_of_int block_width *. player.pos.x) in
    let player_posY = int_of_float (float_of_int block_height *. player.pos.y) in

    let radians = (float_of_int angle) *. (Float.pi /. 180.) in
    let viewVect = { x=sin radians; y=cos radians} in
    viewVect.x <- (viewVect.x *. (float_of_int width)  +. (float_of_int block_width *. player.pos.x ));
    viewVect.y <- (viewVect.y *. (float_of_int height) +. (float_of_int block_height *. player.pos.y ));

    A.draw_line windows_info.draw_area
        ~color:(Draw.opaque Draw.grey)
        ~thick:2
        (int_of_float viewVect.x, int_of_float viewVect.y)
        (player_posX, player_posY);
    
    (* positions de la souris relativement dans le plot *)
    let mouse_pos = {x=viewVect.x /. float_of_int block_width; y=viewVect.y /. float_of_int block_height} in
    let (tileFound, _, touched_pos) = 
        raycast_on_angle level mouse_pos in

    if tileFound then (
        A.fill_rectangle windows_info.draw_area 
            ~color:(Draw.opaque Draw.blue)
            ~w:(block_width)
            ~h:(block_height)
            ((int_of_float touched_pos.x)*block_width, (int_of_float touched_pos.y)*block_height);
        Sdl_area.update windows_info.draw_area;
        W.update windows_info.draw_area_widget;
        W.update windows_info.label;
    );
    ();;

let raycast windows_info level = 
    let view_angle = level.player.view_angle in
    for i = view_angle-25 to view_angle+25 do
        aux_raycast windows_info level i;
    done;
