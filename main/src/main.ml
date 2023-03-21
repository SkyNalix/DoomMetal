open Ast

open Sdlevent


let vector_scalar_mult v s =
    {x=v.x *. s; y=v.y *. s}

let vector_add v1 v2 =
    {x= v1.x +. v2.x; y= v1.y +. v2.y}


let proc_events windows_info (level:level) event : unit = 
    let player = level.player in
    let radians = (float_of_int level.player.view_angle) *. (Float.pi /. 180.) in
    let x_change = sin radians *. 0.1 in
    let y_change = cos radians *. 0.1 in

    let acceleration_factor = 1.0 in

    match event with 
    | KeyDown { keycode = Sdlkeycode.Escape } ->
        Sdl.quit ();
        exit 0
    | KeyDown { keycode = Sdlkeycode.Z } -> 
        player.acceleration.x <- acceleration_factor *. (sin radians);
        player.acceleration.y <- acceleration_factor *. (cos radians);

    | KeyDown { keycode = Sdlkeycode.Q } -> 
        player.acceleration.x <- acceleration_factor *. (cos radians);
        player.acceleration.y <- acceleration_factor *. (-. sin radians);

    | KeyDown { keycode = Sdlkeycode.S } ->
        player.acceleration.x <- -. (acceleration_factor *. (sin radians));
        player.acceleration.y <- -. (acceleration_factor *. (cos radians));

    | KeyDown { keycode = Sdlkeycode.D } -> 
        player.acceleration.x <- acceleration_factor *. (-. cos radians);
        player.acceleration.y <- acceleration_factor *. (sin radians);


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
        Player.shoot y_change x_change level level.player level.enemies  

    | Quit _ ->
        Sdl.quit ();
        exit 0
    | _ -> ()
    

let () =
    let level = Level.get "1" in
    let windows_info = Common.make_default_windows_info level in
    let textures_path = "main/resources/textures/" in
    let textures = [
        ("sky", "sky.bmp");
        ("white_bricks", "white_bricks.bmp" );
        ("red_bricks", "red_bricks.bmp");
    ] in
    let textures = List.map (fun (k, v) -> 
        k, Sdltexture.create_from_surface windows_info.render 
                (Sdlsurface.load_bmp ~filename:(textures_path^v))) textures in

    let fps = 1000/60 in

    let render () = (
        (* Enemy.actionEnemy level; *)
        
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

    let player = level.player in


    let rec main_loop () =
        event_loop ();

        let time_step = 0.1 in
        let friction = 0.9 in

        player.acceleration <- vector_scalar_mult player.acceleration 0.8;

        player.velocity <- vector_add player.velocity (vector_scalar_mult player.acceleration time_step);
        
        player.velocity <- vector_scalar_mult player.velocity friction;

        (* Update the player's position *)
        let pos =  vector_add player.pos (vector_scalar_mult player.velocity time_step) in
        player.pos.x <- pos.x;
        player.pos.y <- pos.y;

        (* Player.update_pos level; *)
        render ();
        Sdltimer.delay ~ms:(fps);
        main_loop ()

    in
    main_loop ()