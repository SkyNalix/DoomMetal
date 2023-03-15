open Ast

open Sdlevent

let pred_mouse_pos = ref (-1);;
let proc_events windows_info (level:level) event : unit = 
    let radians = (float_of_int level.player.view_angle) *. (Float.pi /. 180.) in
    let x_change = sin radians *. 0.1 in
    let y_change = cos radians *. 0.1 in

    let run_speed = 0.05 in
    
    match event with 
    | KeyDown { keycode = Sdlkeycode.Escape } ->
        Sdl.quit ();
        exit 0
    | KeyDown { keycode = Sdlkeycode.Z } -> 
        level.player.forward_speed <- run_speed;
    | KeyDown { keycode = Sdlkeycode.S } ->
        level.player.forward_speed <- -.run_speed;
    | KeyDown { keycode = Sdlkeycode.Q } -> 
        level.player.sideway_speed <- -.run_speed;
    | KeyDown { keycode = Sdlkeycode.D } -> 
        level.player.sideway_speed <- run_speed;
    | KeyDown { keycode = Sdlkeycode.Left } -> 
        level.player.view_angle <- level.player.view_angle + 15;
    | KeyDown { keycode = Sdlkeycode.Right } -> 
        level.player.view_angle <- level.player.view_angle - 15;
    | KeyUp { keycode = Sdlkeycode.Z } | KeyUp { keycode = Sdlkeycode.S } ->
        level.player.forward_speed <- 0.;
    | KeyUp { keycode = Sdlkeycode.Q } | KeyUp { keycode = Sdlkeycode.D } -> 
        level.player.sideway_speed <- 0.;
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

    let update () = (
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
    update ();

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
        update ();
        Sdltimer.delay ~ms:(fps);
        main_loop ()

    in
    main_loop ()