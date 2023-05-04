open Ast

let approach (level:level) (enemy : enemy)  = (* Deplacement float = 0.05, déplacement map = 1 *)
    if(level.player.pos.x >= enemy.pos.x) then ( 
        enemy.pos.x <- enemy.pos.x +. 0.08;
        enemy.pos.y <- enemy.pos.y;
    ) else ( 
        enemy.pos.x <- enemy.pos.x -. 0.08;
        enemy.pos.y <- enemy.pos.y;
    );

    if(level.player.pos.y >= enemy.pos.y) then (
        enemy.pos.x <- enemy.pos.x;
        enemy.pos.y <- enemy.pos.y +. 0.08;
    ) else (
        enemy.pos.x <- enemy.pos.x;
        enemy.pos.y <- enemy.pos.y -. 0.08;
    ) ;
        
    (* check contact pour dégâts *)
    if(Common.arrondir enemy.pos.x level.player.pos.x 0.15 && 
      Common.arrondir enemy.pos.y level.player.pos.y 0.15) 
    then (  
        level.player.hp <- level.player.hp - 5; 
        print_string("You've been dealt 5 HP\n");
        if level.player.hp = 0 then (
            print_string("You lost\n");
            Sdl.quit ();
            exit 0
        )
    )
;;


let isInReach (player:player) (enemy : enemy) = 
  if ( player.pos.x >= enemy.pos.x && player.pos.x <= enemy.pos.x +. 1.5 
      && player.pos.y >= enemy.pos.y && player.pos.y <= enemy.pos.y +. 1.5) then true 
  else( if( player.pos.x <= enemy.pos.x && player.pos.x >= enemy.pos.x -. 1.5 
      && player.pos.y <= enemy.pos.y && player.pos.y >= enemy.pos.y -. 1.5
  )    then true 
  else (if (player.pos.x >= enemy.pos.x && player.pos.x <= enemy.pos.x +. 1.5 &&
      player.pos.y <= enemy.pos.y && player.pos.y >= enemy.pos.y -. 1.5 ) then true  
  else (if (player.pos.x <= enemy.pos.x && player.pos.x >= enemy.pos.x -. 1.5 && 
      player.pos.y >= enemy.pos.y && player.pos.y <= enemy.pos.y +. 1.5) then true else false   
  ) ) ) ;;

let actionEnemy (level : level) = 
  let see_Player enemy = (* Faire en sorte que les mob réagissent en cas de tir *)
      if isInReach level.player enemy then (
          approach level enemy
      )
  in
  List.iter see_Player level.enemies;
;;


let getRenderInfo (player:player) enemy =
    let angle v1 v2 = (
        let dot = (v1.x *. v2.x) +. (v1.y *. v2.y) in
        let n1 = sqrt ((v1.x *. v1.x) +. (v1.y *. v1.y)) in
        let n2 = sqrt ((v2.x *. v2.x) +. (v2.y *. v2.y)) in
        let cos_theta = dot /. (n1 *. n2) in
        let sin_theta = (v1.x *. v2.y -. v1.y *. v2.x) /. (n1 *. n2) in
        (atan2 sin_theta cos_theta) *. 180.0 /. Float.pi ) in

    let dx = (enemy.pos.x -. player.pos.x) in
    let dy = (enemy.pos.y -. player.pos.y) in
    let player_angle_vec = Player.viewAngleAsVec player.view_angle in

    let diff_angle = angle player_angle_vec {x=dx;y=dy} in
    let in_fov = (Float.neg player.fov) < diff_angle && diff_angle <= player.fov in
    let playerEnemyDistance = (
        let x = Float.abs (enemy.pos.x -. player.pos.x) in
        let y = Float.abs (enemy.pos.y -. player.pos.y) in
        sqrt(x *. x +. y *. y)
    ) in {
        fov=player.fov;
        diff_angle=diff_angle;
        in_fov=in_fov;
        playerEnemyDistance=playerEnemyDistance;
        enemy=enemy
    }