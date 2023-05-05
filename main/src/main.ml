open Ast

open Sdlevent

let proc_main_menu_events game event = 
    match event with
    | KeyDown { keycode = Sdlkeycode.Escape } ->
        Sdl.quit ();
        exit 0
    | KeyDown { keycode = Sdlkeycode.Return } ->
        game.state <- PLAYING;
        game.level <- Level.get "1"
    | _ -> ()

let proc_level_finished_events game event = 
    match event with
    | KeyDown { keycode = Sdlkeycode.Escape } ->
        Sdl.quit ();
        exit 0
    | KeyDown { keycode = Sdlkeycode.Return } ->
        game.state <- MAIN_MENU
    | _ ->  ()

let proc_playing_events windows_info (level:level) event = 
    let player = level.player in
    let radians = Common.degToRad player.view_angle in
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
        level.player.view_angle <- mod_float (level.player.view_angle +. 15.) 360.;
    | KeyDown { keycode = Sdlkeycode.Right } -> 
        level.player.view_angle <- mod_float (level.player.view_angle -. 15.) 360.;
        
    | Mouse_Motion e -> 
        if e.mm_xrel < 0 then
            level.player.view_angle <- mod_float (level.player.view_angle +. 1.) 360.
        else if e.mm_xrel > 0 then
            level.player.view_angle <- mod_float (level.player.view_angle -. 1.) 360.;
        Sdlmouse.warp_in_window windows_info.window ~x:500 ~y:500;

    | KeyDown { keycode = Sdlkeycode.Space} ->
        if(!Drawer3D.reload = false) then(
            Drawer3D.reload := true ;  
            print_string "dans if"; 
                
            let time_now = Unix.localtime (Unix.time ()) in 
            Drawer3D.sec_fin := float_of_int time_now.tm_sec +. 2.0;
            Player.shoot y_change x_change level level.player level.enemies  ) 
        else ()

    | Quit _ ->
        Sdl.quit ();
        exit 0
    | _ -> ()
    

let () =
    let game = {
        state = MAIN_MENU;
        level = Level.get "1";
    } in
    let windows_info = Common.make_default_windows_info () in
    let textures_path = "main/resources/textures/" in
    let textures = [
        ("main_menu", "main_menu.bmp");
        ("you_escaped", "you_escaped.bmp");
        ("sky", "sky.bmp");
        ("white_bricks", "white_bricks.bmp" );
        ("red_bricks", "red_bricks.bmp");
        ("door", "door.bmp");
        ("level_end", "level_end.bmp");
        ("enemy", "enemy.bmp");
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

    let renderLevel () = (
        Player.update_pos game;
        (* Enemy.actionEnemy level; *)
        let rays = Raycasting.raycast game.level in
        let rays = List.sort (fun r1 r2 -> 
            if r1.distance > r2.distance then -1
            else if r1.distance < r2.distance then 1
            else 0 ) rays in

        if windows_info.parameters.drawer2D then (
            Drawer2D.render windows_info game.level rays;
        ) else (
            Drawer3D.render windows_info game.level textures rays;
        );
        Sdlrender.render_present windows_info.render;
    ) in

    let renderMainMenu () = (
        Sdlrender.copyEx 
            windows_info.render 
            ~texture:(List.assoc "main_menu" textures)
            ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(1280,720))
            ~dst_rect:(Sdlrect.make ~pos:(0,0) 
                ~dims:(windows_info.width, windows_info.height))
        ();
    ) in

    let renderLevelFinish () = (
        Sdlrender.copyEx 
            windows_info.render 
            ~texture:(List.assoc "you_escaped" textures)
            ~src_rect:(Sdlrect.make ~pos:(0,0) ~dims:(1314,886))
            ~dst_rect:(Sdlrect.make ~pos:(0,0) 
                ~dims:(windows_info.width, windows_info.height))
        ();
    ) in

    let rec event_loop () =
        match Sdlevent.poll_event () with
            | Some ev -> 
                (match game.state with 
                    | MAIN_MENU -> proc_main_menu_events game ev
                    | PLAYING -> proc_playing_events windows_info game.level ev
                    | LEVEL_FINISHED -> proc_level_finished_events game ev
                );
                event_loop ()
            | None -> ()
    in

    let rec main_loop () =
        event_loop ();
        Sdlrender.set_draw_color windows_info.render ~rgb:(72,68,67) ~a:255;
        Sdlrender.clear windows_info.render;
        (match game.state with 
            | MAIN_MENU -> renderMainMenu ()
            | PLAYING -> renderLevel ()
            | LEVEL_FINISHED -> renderLevelFinish ()
        );
        Sdlrender.render_present windows_info.render;
        Sdltimer.delay ~ms:(fps);
        main_loop ()

    in

    let  rec thread_ennemi a = 
        print_string("THREAD \n");
        Thread.delay 0.5 ;
        Enemy.actionEnemy game.level; 
        thread_ennemi a
    in
    let z = Thread.create (thread_ennemi ) 4 in
    main_loop ()
    