open AST

open Sdlevent

let proc_events (level:level) e : level = 
    let radians = (float_of_int level.player.view_angle) *. (Float.pi /. 180.) in
    let x_change = sin radians *. 0.1 in
    let y_change = cos radians *. 0.1 in
    let neg = Float.neg in
    
    match e with 
    | KeyDown { keycode = Sdlkeycode.Escape } ->
        Sdl.quit ();
        exit 0
    | KeyDown { keycode = Sdlkeycode.Z } -> 
        Player.update_player level x_change y_change 0
    | KeyDown { keycode = Sdlkeycode.S } -> 
        Player.update_player level (neg x_change) (neg y_change) 0
    | KeyDown { keycode = Sdlkeycode.Q } -> 
        Player.update_player level y_change (neg x_change) 0
    | KeyDown { keycode = Sdlkeycode.D } -> 
        Player.update_player level (neg y_change) x_change 0
    | KeyDown { keycode = Sdlkeycode.Left } -> 
        Player.update_player level 0. 0. (15)
    | KeyDown { keycode = Sdlkeycode.Right } -> 
        Player.update_player level 0. 0. (-15)

    | Mouse_Motion e -> 
        if e.mm_xrel < 0 then
            Player.update_player level 0. 0. 1
        else
            Player.update_player level 0. 0. (-1)

    | KeyDown { keycode = Sdlkeycode.Space} ->
        Player.shoot y_change x_change level level.player level.enemies  
    | Quit _ ->
        Sdl.quit ();
        exit 0
    | _ -> level


let () =
    let level = Level.get "empty_level" in
    let windows_info = Common.make_default_windows_info level in
    let fps = 1000/60 in

    let draw_2D = windows_info.parameters.drawer2D in

    let update () = (
        (* Enemy.actionEnemy level; *)
        
        Sdlrender.set_draw_color windows_info.render ~rgb:(0,0,0) ~a:255;
        Sdlrender.clear windows_info.render;
        let rays = Raycasting.raycast level in
        let rays = List.sort (fun r1 r2 -> 
            if r1.distance > r2.distance then -1
            else if r1.distance < r2.distance then 1
            else 0 ) rays in

        if draw_2D then (
            Drawer2D.render windows_info level rays;
        ) else (
            Drawer3D.render windows_info level rays;
        );

        Sdlrender.render_present windows_info.render;
    ) in
    update ();

    let rec event_loop level =
        match Sdlevent.poll_event () with
            | Some ev -> 
                let level = proc_events level ev in
                event_loop level
            | None -> level
    in
    let rec main_loop level =
        let level = event_loop level in
        update ();
        Sdltimer.delay ~ms:(fps);
        main_loop level

    in
    main_loop level