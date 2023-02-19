open AST

open Sdlevent


let print_mouse_motion_event e =
    Printf.printf "
  mouse_motion_event ((
    timestamp: %ld
    window_id: %lX
    buttons: %s
    x: %d
    y: %d
    xrel: %d
    yrel: %d
  ))\n%!"
    e.mm_timestamp
    e.mm_window_id
    (String.concat " "
      (List.map string_of_int e.mm_buttons))
    e.mm_x
    e.mm_y
    e.mm_xrel
    e.mm_yrel
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
        Common.update_player level x_change y_change 0
    | KeyDown { keycode = Sdlkeycode.S } -> 
        Common.update_player level (neg x_change) (neg y_change) 0
    | KeyDown { keycode = Sdlkeycode.Q } -> 
        Common.update_player level y_change (neg x_change) 0
    | KeyDown { keycode = Sdlkeycode.D } -> 
        Common.update_player level (neg y_change) x_change 0
    | KeyDown { keycode = Sdlkeycode.Left } -> 
        Common.update_player level 0. 0. (15)
    | KeyDown { keycode = Sdlkeycode.Right } -> 
        Common.update_player level 0. 0. (-15)
    
    | Mouse_Motion e -> 
        if e.mm_xrel < 0 then
            Common.update_player level 0. 0. 1
        else
            Common.update_player level 0. 0. (-1)
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
        Sdlrender.set_draw_color windows_info.render ~rgb:(0,0,0) ~a:255;
        Sdlrender.clear windows_info.render;

        if draw_2D then (
            Drawer2D.drawLevel windows_info level;
            let rays = Raycasting.raycast windows_info level in
            List.iter (fun ray -> Drawer2D.drawRay windows_info level ray) rays;
        ) else (
            let rays = Raycasting.raycast windows_info level in
            List.iter (fun ray -> Drawer3D.drawRay windows_info level ray) rays;
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