open AST



let texture_of_wall = function
    | WALL -> "white_bricks"
    | RED_WALL -> "red_bricks"
    | _ -> "cobblestone";;

let update_player (level:level) x y view_angle : level =
    level.player <- {
        pos = {
            x=(level.player.pos.x +. x);
            y=(level.player.pos.y +. y)
        };
        view_angle = (level.player.view_angle + view_angle) mod 360
    };
    level ;;

let parameters = 
    let open AST in
    let args : parameters = {
        debug = false;
        tmp = false;
    } in
    Array.fold_left
    (fun args arg -> 
        match arg with
        | "--debug" ->
            {args with debug=true}
        | _ -> args
        )
    args
    Sys.argv
    
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
    {
        parameters = parameters;
        window = window;
        render = render;
        height = height;
        width = width ;
        block_width = width / (Array.length level.plot.(0));
        block_height = height / (Array.length level.plot);
    }
)