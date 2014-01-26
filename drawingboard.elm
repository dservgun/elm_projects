{-- Drawing board  --}
{-- The objective of this board is to present a user with a list of selections 
that can be rubberbandable.
--}
import Mouse
import Window
shape = {x = -1, y = -1, w = -1, h = -1}
shapes = []
data DragState = Started | Released
dragState = Released
drawing_bounds = {start_x = -1, start_y = -1, cur_x = -1, cur_y = -1}
updateDragState isDown aState = if | isDown == True -> case aState of
                                            Released -> Started
                                            Started -> Started
                                  | not isDown -> case aState of
                                            Started -> Released
                                            Released -> Released
updateDrawingBounds (state, (x,y)) bounds = case state of
                                          Started -> {bounds | start_x <- if  | bounds.start_x == -1 -> x 
                                                                              | otherwise -> bounds.start_x
                                                               ,start_y <- if | bounds.start_y == -1 -> y
                                                                             | otherwise -> bounds.start_y
                                                               , cur_x <- x
                                                               , cur_y <- y}
                                                               
                                          Released -> {start_x = -1, start_y = -1, cur_x = -1, cur_y = -1}
drawRect (aState,_) = 
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
main = let 
            currentDragState = foldp updateDragState dragState Mouse.isDown
            bounds = foldp updateDrawingBounds drawing_bounds(lift2 (,) currentDragState Mouse.position)
       in            
            drawRect <~ (lift2 (,) bounds currentDragState)

message = [markdown|
A rubber banding example: drag the mouse anywhere on the screen.
|]