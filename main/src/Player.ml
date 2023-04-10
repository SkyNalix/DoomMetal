open Ast

let rec shoot y_change x_change level (player:player) enemies = 
  let px = player.pos.x in
  let py = player.pos.y in
  let rec aux liste  =
      match liste with
      | [] -> ()
      | enn  :: l ->  
          if Common.arrondir enn.pos.y py 0.35 && (Common.arrondir enn.pos.x px 0.35) then (
              enn.hp <- enn.hp -1; 
              level.enemies <- List.filter (fun enn -> enn.hp > 0) level.enemies
          ) else aux l
  in

  if py < 8. && py > 0. && px > 0. && px < 8. && (Common.toucheUnMur px py level) = false then (
      aux enemies;
      let player = {
          player with 
          pos = {
              x=(px +. x_change);
              y=(py +. y_change);
          }
      } in
      shoot y_change x_change level player enemies 
  );
;;

let update_pos level =
    let vector_scalar_mult v s =
        {x=v.x *. s; y=v.y *. s} in
    
    let vector_add v1 v2 =
        {x= v1.x +. v2.x; y= v1.y +. v2.y} in

    let player = level.player in
    let time_step = 0.005 in

        let player_x = int_of_float level.player.pos.x in
        let player_y = int_of_float level.player.pos.y in

        let friction = Common.friction_of_floor_tile level.map.floor.(player_y).(player_x) in
        let radians = (float_of_int level.player.view_angle) *.( Float.pi /. 180.) in

        (* let player_dir = player's view direction, as a vector *)
        let move_vec = {x=player.acceleration.x;y=player.acceleration.y} in
      
        (* calculate the x and y components of the movement vector, as described in the previous answer *)
        let player_angle = radians in (* angle between player's view direction and x-axis *)
        let cos_angle = cos player_angle in
        let sin_angle = sin player_angle in
        let move_x = (move_vec.x *. cos_angle) +. (move_vec.y *. sin_angle) in
        let move_y = (move_vec.y *. cos_angle) -. (move_vec.x *. sin_angle) in
      
        (* calculate the player's new velocity based on the movement vector and acceleration *)
        let accel_vec = {x=move_x; y=move_y} in
        let new_vel = { x = player.velocity.x +. accel_vec.x; y = player.velocity.y +. accel_vec.y } in
      
        (* apply friction to the player's velocity *)
        let friction_vec = { x = player.velocity.x *. friction; y = player.velocity.y *. friction } in
        let final_vel = { x = new_vel.x -. friction_vec.x; y = new_vel.y -. friction_vec.y } in

        player.velocity.x <- final_vel.x;
        player.velocity.y <- final_vel.y;

        let new_pos = vector_scalar_mult player.velocity (time_step) in
        let new_pos = {
            x=Float.round (new_pos.x *. 100.) /. 100.;
            y=Float.round (new_pos.y *. 100.) /. 100.;
        } in

        let new_pos_colision =vector_add player.pos (vector_scalar_mult new_pos (1.4)) in
        let new_pos_int_x = int_of_float new_pos_colision.x in
        let new_pos_int_y = int_of_float new_pos_colision.y in
        if level.map.plot.(new_pos_int_y).(new_pos_int_x) <> NOTHING then (
            if level.map.plot.(new_pos_int_y).(player_x) = NOTHING then (
                new_pos.x <- 0.;
            ) else if level.map.plot.(player_y).(new_pos_int_x) = NOTHING then (
                new_pos.y <- 0.;
            ) else (
                new_pos.x <- 0.;
                new_pos.y <- 0.;
            )
        );
        player.pos.x <- player.pos.x +. new_pos.x;
        player.pos.y <- player.pos.y +. new_pos.y;
    
    ();;

