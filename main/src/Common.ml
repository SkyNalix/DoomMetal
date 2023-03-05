open AST


let floatToInt x = 
    let sub = int_of_float(x) in 
    if (x -. float_of_int(sub) >= 0.5 ) then sub+1 else sub 
;;

let toucheUnMur x y (level : level)=
    match level.plot.(floatToInt y).(floatToInt x) with 
    | NOTHING -> false 
    | _ -> true
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