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

let drawLevel windows_info level : unit = 
    let plot : tile array array = level.plot in
    let player = level.player.pos in

    let block_width = (windows_info.width / (Array.length plot.(0))) in
    let block_height = (windows_info.height / (Array.length plot)) in
    let plot_width = Array.length plot.(0) in
    let plot_height = Array.length plot in

    A.fill_rectangle windows_info.draw_area
        ~color:(Draw.opaque Draw.black)
        ~w:windows_info.width
        ~h:windows_info.height
        (0,0);

    let rec drawPlot y x : unit =
        A.fill_rectangle windows_info.draw_area 
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
    
        A.fill_circle windows_info.draw_area 
        ~color:(Draw.opaque Draw.green) 
        ~radius:5
        (x_full*block_height + x2 ,
         y_full*block_width + y2);
        ()
    in

    drawPlot 0 0 ;
    drawPlayer () ;
    ();;
