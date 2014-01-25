{-- Drawing board  --}
import Mouse
import Window
shape = {x = -1, y = -1, w = -1, h = -1}
shapes = []
initial = {down = False, start_x = -1, start_y = -1, cur_x = -1, cur_y = -1}

updateState (mouseDown, (xpos,ypos)) state =
  {state |  down <- mouseDown,
            start_x  <- if | mouseDown == True && (state.down == False) -> xpos
                         | mouseDown == False -> -1
                         | mouseDown && state.down -> state.start_x
           , start_y <- if | mouseDown && (state.down == False) -> ypos
                         | mouseDown && state.down -> state.start_y
                         | mouseDown == False -> -1
           , cur_x <-  if | mouseDown == False -> -1 | otherwise -> xpos
           , cur_y <-  if | mouseDown == False -> -1 | otherwise -> ypos           
   }

updateShapes aState = aState :: shapes   
drawRect aState = 
                let 
                      cw = abs <| aState.start_x - aState.cur_x
                      ch = abs <| aState.start_y - aState.cur_y
                      s = rect (toFloat cw)
                          (toFloat ch)
                        |> filled blue
                        |> move (toFloat <| (0 + aState.start_x), toFloat <| (0 - aState.start_y))
                      c = collage (cw) (ch) [s]                      
                    in 
                      layers [c, message, asText aState]
                      
--main = asText <~ foldp updateState initial (lift2 (,) Mouse.isDown Mouse.position)                                           
main = drawRect <~ foldp updateState initial (lift2 (,) Mouse.isDown Mouse.position)

message = [markdown|
A rubber banding example: drag the mouse anywhere on the screen.
|]