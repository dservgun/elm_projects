{-- Drawing board  --}
import Mouse
import Window

initial = {down = False, start_x = -1, start_y = -1, cur_x = -1, cur_y = -1, 
          shapes = []}

updateState (mouseDown, (xpos,ypos)) state =
  {state |  down <- mouseDown,
            start_x  <- if | mouseDown == True && (state.down == False) -> xpos
                         | mouseDown == False -> -1
                         | mouseDown && state.down -> state.start_x
           , start_y <- if | mouseDown && (state.down == False) -> ypos
                         | mouseDown && state.down -> state.start_y
                         | mouseDown == False -> -1
           , cur_x <- if | mouseDown == False -> -1 | otherwise -> xpos
           , cur_y <- if | mouseDown == False -> -1 | otherwise -> ypos
   }

drawRect (w, h) aState = 
                let 
                      cw = abs <| aState.cur_x - aState.start_x
                      ch = abs <| aState.cur_y - aState.start_y
                      s = rect (toFloat (aState.cur_x - aState.start_x)) 
                        (toFloat (aState.cur_y - aState.start_y))
                        |> filled blue
                        |> move (toFloat aState.start_x, toFloat <| -1 * aState.start_y)
                      c = collage cw ch [s]
                    in 
                      layers [c, message, asText aState]
                      
--main = asText <~ foldp updateState initial (lift2 (,) Mouse.isDown Mouse.position)                                           
main = drawRect <~ Window.dimensions ~ foldp updateState initial (lift2 (,) Mouse.isDown Mouse.position)

message = [markdown|
A rubber banding example: drag the mouse anywhere on the screen.
|]