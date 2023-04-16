open Ast

open Sdlevent


let vector_scalar_mult v s =
    {x=v.x *. s; y=v.y *. s}

let vector_add v1 v2 =
    {x= v1.x +. v2.x; y= v1.y +. v2.y}

let shoot = ref false ;; 
let sec_fin = ref 0.0;;

let proc_events windows_info (level:level) event : unit = 
    let player = level.player in
    let radians = (float_of_int level.player.view_angle) *. (Float.pi /. 180.) in
    let x_change = sin radians *. 0.1 in
    let y_change = cos radians *. 0.1 in

    match event with 
    | KeyDown { keycode = Sdlkeycode.Escape } ->
        Sdl.quit ();
        exit 0
    | KeyDown { keycode = Sdlkeycode.Z } -> 
        player.acceleration.y <- 1.;

    | KeyDown { keycode = Sdlkeycode.Q } -> 
        player.acceleration.x <- 1.;

    | KeyDown { keycode = Sdlkeycode.S } ->
        player.acceleration.y <- -. 1.;

    | KeyDown { keycode = Sdlkeycode.D } -> 
        player.acceleration.x <- -. 1.;

    | KeyUp { keycode = Sdlkeycode.Z } | KeyUp { keycode = Sdlkeycode.S }->
        player.acceleration.y <- 0.;
    | KeyUp { keycode = Sdlkeycode.Q } | KeyUp { keycode = Sdlkeycode.D }->
        player.acceleration.x <- 0.;


    | KeyDown { keycode = Sdlkeycode.Left } -> 
        level.player.view_angle <- level.player.view_angle + 15;
    | KeyDown { keycode = Sdlkeycode.Right } -> 
        level.player.view_angle <- level.player.view_angle - 15;
        
    | Mouse_Motion e -> 
        if e.mm_xrel < 0 then
            level.player.view_angle <- level.player.view_angle + 1
        else if e.mm_xrel > 0 then
            level.player.view_angle <- level.player.view_angle - 1;
        Sdlmouse.warp_in_window windows_info.window ~x:500 ~y:500;

    | KeyDown { keycode = Sdlkeycode.Space} ->
       shoot := true;
       Player.shoot y_change x_change level level.player level.enemies  

    | Quit _ ->
        Sdl.quit ();
        exit 0
    | _ -> ()
    

let () =
    let level = Level.get "1" in
    let windows_info = Common.make_default_windows_info () in
    let textures_path = "main/resources/textures/" in
    let textures = [
        ("sky", "sky.bmp");
        ("white_bricks", "white_bricks.bmp" );
        ("red_bricks", "red_bricks.bmp");
        ("hud", "hud.bmp");
        ("arme", "arme.bmp");
        ("20PV", "20PV.bmp");
        ("15PV", "15PV.bmp");
        ("10PV", "10PV.bmp");
        ("5PV", "5PV.bmp");
        ("0PV", "0PV.bmp");
        ("shotgun_blast","shotgun_blast.bmp");
    ] in
    let textures = List.map (fun (k, v) -> 
        k, Sdltexture.create_from_surface windows_info.render 
                (Sdlsurface.load_bmp ~filename:(textures_path^v))) textures in

    let fps = 1000/60 in

    let render () = (

        Sdlrender.set_draw_color windows_info.render ~rgb:(72,68,67) ~a:255;
        Sdlrender.clear windows_info.render;
        let rays = Raycasting.raycast level in
        let rays = List.sort (fun r1 r2 -> 
            if r1.distance > r2.distance then -1
            else if r1.distance < r2.distance then 1
            else 0 ) rays in

        if windows_info.parameters.drawer2D then (
            Drawer2D.render windows_info level rays;
        ) else (
            Drawer3D.render windows_info level textures rays;
            if (!shoot = true) then (Drawer3D.renderShotgunBlast windows_info; shoot := false;) 
            
            
        );
        Sdlrender.render_present windows_info.render;
    ) in
    render ();

    let rec event_loop () =
        match Sdlevent.poll_event () with
            | Some ev -> 
                proc_events windows_info level ev;
                event_loop ()
            | None -> ()
    in

    let rec main_loop () =
        
        event_loop ();
        Player.update_pos level;
        render ();
        Sdltimer.delay ~ms:(fps);
        main_loop ()

    in

    let  rec thread_ennemi a = 
        print_string("THREAD \n");
        Thread.delay 0.5 ;
        Enemy.actionEnemy level; 
        thread_ennemi a
    in
    let z = Thread.create (thread_ennemi ) 4 in 

    main_loop ()