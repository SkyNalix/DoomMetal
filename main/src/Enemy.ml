open AST



let deplacement (level:level) (enemy : enemy)  = (* Deplacement float = 0.05, déplacement map = 1 *)

        if(level.player.pos.x >= enemy.pos.x) then( 
            enemy.pos <- {
                x = enemy.pos.x +. 0.08;
                y = enemy.pos.y;
            }; )
            else ( 
                enemy.pos <- {
                x = enemy.pos.x -. 0.08;
                y = enemy.pos.y;
            };
            ) ;

        if(level.player.pos.y >= enemy.pos.y) then (
            enemy.pos <- {
                x = enemy.pos.x;
                y = enemy.pos.y +. 0.08;
            };
         )     
        else (
            enemy.pos <- {
                x = enemy.pos.x;
                y = enemy.pos.y -. 0.08;
            };
        ) ;
            
        (* check contact pour dégâts *)
        if(Common.arrondir enemy.pos.x level.player.pos.x 0.15 && 
          Common.arrondir enemy.pos.y level.player.pos.y 0.15) 
        then (  
            level.player <- {
                    pos = {
                x=(level.player.pos.x );
                y=(level.player.pos.y )
            };
            view_angle = (level.player.view_angle );
            hp = level.player.hp - 5;
            }; 
            print_string("vous avez perdu 5 hp \n");
            if level.player.hp = 0 then (
                print_string("perdu \n");
                Sdl.quit ();
                exit 0
            )
        )
;;


let aporte (player:player) (enemy : enemy) = 
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
  let see_Player enemy = (* Faire en sorte que les mob réagissent en cas de tir*)
      if aporte level.player enemy then (
          print_string("deplacement \n");
          deplacement level enemy
      ) else print_string("pas vue \n");
  in
  List.iter see_Player level.enemies;
;;

