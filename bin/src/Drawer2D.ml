open AST
open Bogue
module W = Widget
module L = Layout
module T = Trigger
module A = Sdl_area
module S = Style


let color_of_wall = function
    | WALL -> Draw.white
    | REDWALL -> Draw.red
    | _ -> Draw.black;;

let drawRay windows_info level ray = 
    
    if not ray.rayTouched then () else

    let player = level.player in

    let player_posX = int_of_float (float_of_int windows_info.block_width *. player.pos.x) in
    let player_posY = int_of_float (float_of_int windows_info.block_height *. player.pos.y) in
    A.draw_line windows_info.area2D
        ~color:(Draw.opaque Draw.grey)
        ~thick:2
        (int_of_float ray.angle_vec.x, int_of_float ray.angle_vec.y)
        (player_posX, player_posY);

    A.fill_rectangle windows_info.area2D 
        ~color:(Draw.opaque Draw.blue)
        ~w:(windows_info.block_width)
        ~h:(windows_info.block_height)
        ((int_of_float ray.touched_pos.x)*windows_info.block_width, 
            (int_of_float ray.touched_pos.y)*windows_info.block_height);
    ();;

let drawLevel windows_info level : unit = 
    let plot : tile array array = level.plot in
    let player = level.player.pos in

    let block_width = (windows_info.width / (Array.length plot.(0))) in
    let block_height = (windows_info.height / (Array.length plot)) in
    let plot_width = Array.length plot.(0) in
    let plot_height = Array.length plot in

    let rec drawPlot y x : unit =
        A.fill_rectangle windows_info.area2D 
            ~color:(Draw.opaque (color_of_wall plot.(y).(x))) 
            ~w:(block_width)
            ~h:(block_height)
            (x*block_width, y*block_height);
        let (y,x) = if x >= plot_width-1 then (y+1,0) else (y,x+1) in
        if y >= plot_height then () else drawPlot y x
    in

    let drawPlayer () : unit =
        let x_full = int_of_float player.x in
        let y_full = int_of_float player.y in
        let x_decimal = player.x -. (float_of_int x_full) in 
        let y_decimal = player.y -. (float_of_int y_full) in 
        let x2 = ((int_of_float (x_decimal *. 100.))*block_width)/100 in
        let y2 = ((int_of_float (y_decimal *. 100.))*block_height)/100 in
    
        A.fill_circle windows_info.area2D 
        ~color:(Draw.opaque Draw.green) 
        ~radius:5
        (x_full*block_height + x2 ,
         y_full*block_width + y2);
        ()
    in

    drawPlot 0 0 ;
    drawPlayer ();
    ();;
