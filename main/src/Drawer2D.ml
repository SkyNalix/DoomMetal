open AST

let drawRay windows_info level ray = 
    
    if not ray.rayTouched then () else

    let player = level.player in

    let player_posX = int_of_float (float_of_int windows_info.block_width *. player.pos.x) in
    let player_posY = int_of_float (float_of_int windows_info.block_height *. player.pos.y) in    
    
    Sdlrender.set_draw_color windows_info.render ~rgb:(0,0,255) ~a:255 ;
    let rect = Sdlrect.make 
            ~pos:((int_of_float ray.touched_pos.x)*windows_info.block_width, 
                (int_of_float ray.touched_pos.y)*windows_info.block_height)
            ~dims:(windows_info.block_width, windows_info.block_height) in
    Sdlrender.fill_rect windows_info.render rect;

    let (r,g,b) = Draw.grey in
    Sdlrender.set_draw_color windows_info.render ~rgb:(r,g,b) ~a:255 ;
    Sdlrender.draw_line2 
        windows_info.render
        ~p1:(int_of_float ray.angle_vec.x, int_of_float ray.angle_vec.y)
        ~p2:(player_posX, player_posY);

    (*
    A.fill_rectangle windows_info.area2D 
        ~color:(Draw.opaque Draw.blue)
        ~w:(windows_info.block_width)
        ~h:(windows_info.block_height)
        ((int_of_float ray.touched_pos.x)*windows_info.block_width, 
        (int_of_float ray.touched_pos.y)*windows_info.block_height);

    A.draw_line windows_info.area2D
        ~color:(Draw.opaque Draw.grey)
        ~thick:2
        (int_of_float ray.angle_vec.x, int_of_float ray.angle_vec.y)
        (player_posX, player_posY);
    *)
        
    ();;

let drawLevel windows_info level : unit = 
    let plot : tile array array = level.plot in
    let player = level.player.pos in

    let block_width = (windows_info.width / (Array.length plot.(0))) in
    let block_height = (windows_info.height / (Array.length plot)) in
    let plot_width = Array.length plot.(0) in
    let plot_height = Array.length plot in

    let rec drawPlot y x : unit =
        let tile = plot.(y).(x) in
        let (r,g,b) = if tile = TRANSPARENT_WALL then Draw.green else color_of_wall tile in  

        Sdlrender.set_draw_color windows_info.render ~rgb:(r,g,b) ~a:255 ;
        let rect = Sdlrect.make 
                ~pos:(x*block_width, y*block_height)
                ~dims:(block_width, block_height) in
        Sdlrender.fill_rect windows_info.render rect;
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
    
        Sdlrender.draw_point
            windows_info.render (x_full*block_height + x2 ,
            y_full*block_width + y2);

        (* A.fill_circle windows_info.area2D 
        ~color:(Draw.opaque Draw.green) 
        ~radius:5
        (x_full*block_height + x2 ,
         y_full*block_width + y2); *)
        ()
    in

    drawPlot 0 0 ;
    drawPlayer ();
    ();;