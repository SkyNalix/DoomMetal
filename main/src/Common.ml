open AST


let aporte (player:player) (enemy : enemy) = 
    if ( player.pos.x >= enemy.pos.x && player.pos.x <= enemy.pos.x +. 1.5 
        && player.pos.y >= enemy.pos.y && player.pos.y <= enemy.pos.y +. 1.5) then true 
    else( if( player.pos.x <= enemy.pos.x && player.pos.x >= enemy.pos.x -. 1.5 
        && player.pos.y <= enemy.pos.y && player.pos.y >= enemy.pos.y -. 1.5
    )    then true 
    else (if (player.pos.x >= enemy.pos.x && player.pos.x <= enemy.pos.x +. 1.5 &&
        player.pos.y <= enemy.pos.y && player.pos.y >= enemy.pos.y -. 1.5 ) then true  
    else (if (player.pos.x <= enemy.pos.x && player.pos.x >= enemy.pos.x -. 1.5 && 
        player.pos.y >= enemy.pos.y && player.pos.y <= enemy.pos.y +. 1.5) then true else false   
    ) ) ) 
;;
let myIntToFloat x = (* regarde si on doit arondir au supérieur ou inférieur DANS CERTAIN CAS pour le déplacement*)
    let y = int_of_float(x) in 
    let z = x -. float_of_int(y) in 
    if z >= 0.05 then y+1 else if z <= -0.1 then y-1 else y 
;;

let floatToInt x = 
    let sub = int_of_float(x) in 
    if (x -. float_of_int(sub) >= 0.5 ) then sub+1 else sub 
;;

let toucheUnMur x y (level : level)=
    let tx = floatToInt x in 
    let ty = floatToInt y in 
    (*print_string("what's happening here ? : ") ; print_int( floatToInt x); print_string("\n"); *)
    
    let tuile = level.plot.(tx).(ty) in 
    match tuile with 
    | NOTHING -> print_string("NOTHING \n"); false 
    | _ -> print_string("MUR \n"); true
;;


let print_Position (pos : position)  (str : string) =
    print_string(str );
    print_float(pos.x);
    print_string("    ");
    print_float(pos.y);
    print_string(" \n");
    ()
;;


let arrondir (x : float) (valeur : float ) (arrondit : float) =  (* arrondit pour savoir si on touche quelqu'un -1 -0.6    *)
    x +. 0.35 > arrondit && x -. arrondit < valeur
;;



let texture_of_wall = function
    | WALL -> "white_bricks"
    | RED_WALL -> "red_bricks"
    | _ -> "cobblestone";;

let parameters = 
    let open AST in
    let args : parameters = {
        debug = false;
        drawer2D = false;
    } in
    Array.fold_left
    (fun args arg -> 
        match arg with
        | "--debug" ->
            {args with debug=true}
        | "--2D" -> {args with drawer2D=true}
        | _ -> args
        )
    args
    Sys.argv;;
    
let make_default_windows_info level : windows_info = (
    let width, height = (500, 500) in
    Sdl.init [`VIDEO];
    at_exit print_newline;
    let window, render =
      Sdlrender.create_window_and_renderer
        ~width ~height
        ~flags:[
            Sdlwindow.Input_Grabbed;
            Sdlwindow.Input_Focus;
            Sdlwindow.Mouse_Focus;
            Sdlwindow.FullScreen_Desktop;
      ]
    in
    ignore (window);

    Sdlrender.set_draw_color render ~rgb:(120,120,120) ~a:255;
    Sdlrender.clear render;
    Sdlmouse.show_cursor ~toggle:false;
    let width, height = Sdlwindow.get_size window in
    let drawer3D_height, drawer3D_width = height, width in
    (* let drawer3D_height, drawer3D_width = height, min height width in *)
    let drawer2D_height, drawer2D_width = height, min height width in

    {
        parameters = parameters;
        window = window;
        render = render;
        height = height;
        width = width ;
        drawer3D_height = drawer3D_height;
        drawer3D_width = drawer3D_width;
        drawer2D_height = drawer2D_height;
        drawer2D_width = drawer2D_width;
        block_width = drawer2D_width / (Array.length level.plot.(0));
        block_height = drawer2D_height / (Array.length level.plot);
    }
)