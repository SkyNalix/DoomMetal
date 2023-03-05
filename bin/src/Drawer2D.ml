open AST
open Bogue
module W = Widget
module L = Layout
module T = Trigger
module A = Sdl_area
module S = Style


let color_of_wall = function
    | WALL -> Draw.white
    | RED_WALL -> Draw.red
    | _ -> Draw.black;;
    

let  moblist = ref []   
let booleanMob = ref false    



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

    let myIntToFloat x = (* regarde si on doit arondir au supérieur ou inférieur DANS CERTAIN CAS pour le déplacement*)
    let y = int_of_float(x) in 
    
    let z = x -. float_of_int(y) in 
    if z >= 0.05 then y+1 else if z <= -0.1 then y-1 else y 

    ;;    

let drawLevel windows_info level : unit = 

    if !booleanMob = false then (* Sinon on a un import cyclique*)
    (
        
    let ennemi   = { posE = {x = 5.5; y = 6.;}; nom = "ennemi1"; hp = ref 1; } in 
    moblist := !moblist @ [ennemi]  ;

    (*let ennemi2  = { posE = {x = 7.5; y = 7.5;}; nom = "ennemi2"; hp = ref 1; } in 
    let ennemi3  = { posE = {x = 4.5; y = 3.5;}; nom = "ennemi3"; hp = ref 1; } in 
    let ennemi4  = { posE = {x = 6.2; y = 3.3;}; nom = "ennemi4"; hp = ref 1; } in 
    let ennemi5  = { posE = {x = 4.5; y = 2.5;}; nom = "ennemi5"; hp = ref 1; } in 
    let ennemi6  = { posE = {x = 2.5; y = 7.5;}; nom = "ennemi6"; hp = ref 1; } in 
    

    
    moblist := !moblist @ [ennemi2] ;
    moblist := !moblist @ [ennemi3] ;
    moblist := !moblist @ [ennemi4] ;
    moblist := !moblist @ [ennemi5] ;
    moblist := !moblist @ [ennemi6] ; *)
    
    print_string("moblist Drawer2D : ");
    print_int(List.length !moblist);
    print_string("\n");

    booleanMob := true; ) ;

    let plot : tile array array = level.plot in
    let player = level.player.pos in

    let block_width = (windows_info.width / (Array.length plot.(0))) in
    let block_height = (windows_info.height / (Array.length plot)) in
    let plot_width = Array.length plot.(0) in
    let plot_height = Array.length plot in

    let rec drawPlot y x : unit =
        let tile = plot.(y).(x) in
        let color = if tile = TRANSPARENT_WALL then Draw.green else color_of_wall tile in  
        A.fill_rectangle windows_info.area2D 
        ~color:(Draw.opaque color) 
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


    let rec drawEnnemi (liste : ennemi list) () : unit = 
        match liste with 
        | [] -> ()
        | enn :: l ->
            let x_full = int_of_float enn.posE.x in
            let y_full = int_of_float enn.posE.y in
            let x_decimal = enn.posE.x -. (float_of_int x_full) in 
            let y_decimal = enn.posE.y -. (float_of_int y_full) in 
            let x2 = ((int_of_float (x_decimal *. 100.))*block_width)/100 in
            let y2 = ((int_of_float (y_decimal *. 100.))*block_height)/100 in

            A.fill_circle windows_info.area2D 
            ~color:(Draw.opaque Draw.red) 
            ~radius:5
            (x_full*block_height + x2 ,
            y_full*block_width + y2);
            drawEnnemi l ()      
    in

    

    drawPlot 0 0 ;
    drawPlayer ();
    drawEnnemi !moblist ();
    ();;

