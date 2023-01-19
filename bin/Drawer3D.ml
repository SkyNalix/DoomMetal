open AST
open Bogue
module W = Widget
module L = Layout
module T = Trigger
module A = Sdl_area
module S = Style


let drawRay windows_info level ray i = 
    
    if not (ray.rayTouched) then (failwith "ray not touched") else

    (* let win_step = windows_info.width / (ray.angle_max - ray.angle_min) in  *)
    let win_step = windows_info.width / (ray.angle_max - ray.angle_min) in 
    let rect_height = (int_of_float ( float_of_int(windows_info.height) /. ray.distance)) in

    let t = ray.angle_max - ray.angle in
    let rec_start = (
        t * win_step,
        windows_info.height/2 - rect_height/2
    ) in

    A.fill_rectangle windows_info.area3D
        ~color:(Draw.opaque (Drawer2D.color_of_wall 
                level.plot.(int_of_float ray.touched_pos.y).(int_of_float ray.touched_pos.x)) )
        (* ~color:(Draw.random_color ()) *)
        ~w:win_step
        ~h:rect_height
        rec_start;
    ();;