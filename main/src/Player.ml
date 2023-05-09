open Ast

let shoot level = 
  let enemies_info = List.map (Enemy.computeInfo level.player) level.enemies in
  let enemies_info = List.filter (fun info -> info.in_fov
                    && -.5. < info.diff_angle && info.diff_angle < 5. 
                    && (
                      let enemy_angle = level.player.view_angle +. info.diff_angle in
                      let enemy_angleVec = Common.angleAsVec enemy_angle in
                      let (_,distance,_,_) = Raycasting.raycast_on_angle level enemy_angleVec in
                      info.playerEnemyDistance < distance
                    )  ) enemies_info in
  List.iter (fun info ->
      info.enemy.hp <- info.enemy.hp -1; 
    ) enemies_info;
  level.enemies <- List.filter (fun enn -> enn.hp > 0) level.enemies;;



let update_pos game =
    let level = Option.get game.level in
    let vector_scalar_mult v s =
        {x=v.x *. s; y=v.y *. s} in
    
    let vector_add v1 v2 =
        {x= v1.x +. v2.x; y= v1.y +. v2.y} in

    let player = level.player in
    let player_x = int_of_float level.player.pos.x in
    let player_y = int_of_float level.player.pos.y in
    let time_step = 0.005 in

    let player_angle = Common.degToRad player.view_angle in

    let friction = Common.friction_of_floor_tile level.map.floor.(player_y).(player_x) in
    let move_vec = {x=player.acceleration.x;y=player.acceleration.y} in
    
    let cos_angle = cos player_angle in
    let sin_angle = sin player_angle in
    
    let accel_vec = {
        x=(move_vec.x *. cos_angle) +. (move_vec.y *. sin_angle);
        y=(move_vec.y *. cos_angle) -. (move_vec.x *. sin_angle)
    } in
    let new_vel = vector_add player.velocity accel_vec in
    
    let friction_vec = vector_scalar_mult player.velocity friction in
    player.velocity.x <- new_vel.x -. friction_vec.x;
    player.velocity.y <- new_vel.y -. friction_vec.y;

    let new_pos = vector_scalar_mult player.velocity (time_step) in
    let new_pos = {
        x=Float.round (new_pos.x *. 100.) /. 100.;
        y=Float.round (new_pos.y *. 100.) /. 100.;
    } in

    let new_pos_colision =vector_add player.pos (vector_scalar_mult new_pos 1.4) in
    let new_pos_int_x = int_of_float new_pos_colision.x in
    let new_pos_int_y = int_of_float new_pos_colision.y in
    if level.map.plot.(new_pos_int_y).(new_pos_int_x) <> NOTHING then (
        Level.touchedBlockAction game new_pos_int_x new_pos_int_y;
    );
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

