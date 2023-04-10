open Ast


let white = 255,255,255;;
let black = 0,0,0;;
let red = 255,0,0;;
let blue = 0,0,255;;
let grey = 105,105,105;;
let green = 0,255,0;;
let color_of_wall = function
    | WALL -> white
    | RED_WALL -> red
    | TRANSPARENT_WALL -> green
    | _ -> black;;


let render windows_info level rays = 
    let plot = level.map.plot in
    let player_pos = level.player.pos in

    let block_width = (windows_info.drawer2D_width / level.map.width) in
    let block_height = (windows_info.drawer2D_height / level.map.height) in

    let drawRay ray = 
        
        if not ray.rayTouched then () else

        let player_posX = int_of_float (float_of_int block_width *. player_pos.x) in
        let player_posY = int_of_float (float_of_int block_height *. player_pos.y) in    
        
        Sdlrender.set_draw_color windows_info.render ~rgb:blue ~a:255 ;
        let rect = Sdlrect.make 
                ~pos:((int_of_float ray.touched_pos.x)*block_width, 
                    (int_of_float ray.touched_pos.y)*block_height)
                ~dims:(block_width, block_height) in
        Sdlrender.fill_rect windows_info.render rect;

        Sdlrender.set_draw_color windows_info.render ~rgb:grey ~a:255 ;
        Sdlrender.draw_line2 
            windows_info.render
            ~p1:(
                int_of_float (ray.angle_vec.x *. (float_of_int block_width)), 
                int_of_float (ray.angle_vec.y *. (float_of_int block_height)))
            ~p2:(player_posX, player_posY);
    in

    let rec drawPlot y x : unit =
        let tile = plot.(y).(x) in
        let color = color_of_wall tile in  

        Sdlrender.set_draw_color windows_info.render ~rgb:color ~a:255 ;
        let rect = Sdlrect.make 
                ~pos:(x*block_width, y*block_height)
                ~dims:(block_width, block_height) in
        Sdlrender.fill_rect windows_info.render rect;
        let (y,x) = if x >= level.map.width-1 then (y+1,0) else (y,x+1) in
        if y >= level.map.height then () else drawPlot y x
    in

    let drawLiving pos color : unit =
        let x_full = int_of_float pos.x in
        let y_full = int_of_float pos.y in
        let x_decimal = pos.x -. (float_of_int x_full) in 
        let y_decimal = pos.y -. (float_of_int y_full) in 
        let x2 = ((int_of_float (x_decimal *. 100.))*block_width)/100 in
        let y2 = ((int_of_float (y_decimal *. 100.))*block_height)/100 in

        Sdlrender.set_draw_color windows_info.render ~rgb:color ~a:255 ;
        let rect = Sdlrect.make 
                ~pos:(x_full*block_height + x2, y_full*block_width + y2)
                ~dims:(block_width/10, block_height/10) in
        Sdlrender.fill_rect windows_info.render rect
    in

    drawPlot 0 0 ;
    List.iter (fun ray -> drawRay ray) rays;
    drawLiving player_pos white;
    List.iter (fun enemy -> drawLiving enemy.pos red) level.enemies;
    ();;