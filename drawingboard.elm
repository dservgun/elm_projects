{-- Drawing board  --}
{-- The objective of this board is to present a user with a list of selections 
that can be rubberbandable.
--}
import Mouse
import Window

shape = {x = -1, y = -1, w = -1, h = -1}
shapes = []
data DragState = Started | Released
collage_width = 500
collage_height = 500                  

dragState = Released
drawing_bounds = {state = Released, start_x = -1, start_y = -1, cur_x = -1, cur_y = -1}
areEqual s1 s2 = (s1.start_x == s2.start_x && s1.start_y == s2.start_y 
                    && s1.cur_x == s2.cur_x && s1.cur_y == s2.cur_y)
isValid s = s.start_x > 0 || s.start_y > 0 || s.cur_x > 0 || s.cur_y > 0

updateShapes (s, aState) aList = 
    case aState of
    Released -> case aList of 
                    [] -> if | isValid s -> s :: aList
                             | otherwise -> aList
                    h::t -> if 
                            | not (areEqual h s) -> s :: aList
                            | otherwise -> aList
    Started -> aList
                               
                                          
                                             
updateDragState isDown aState = if | isDown == True -> case aState of
                                            Released -> Started
                                            Started -> Started
                                  | not isDown -> case aState of
                                            Started -> Released
                                            Released -> Released
updateDrawingBounds (iState, (x,y)) bounds = case iState of
                                          Started -> {bounds | start_x <- case bounds.state of
                                                                    Released -> x
                                                                    _ -> bounds.start_x
                                                               ,start_y <- case bounds.state of
                                                                    Released -> x
                                                                    _ -> bounds.start_y
                                                               , cur_x <- x
                                                               , cur_y <- y
                                                               , state <- iState}                                                              
                                          Released -> {bounds |
                                                        start_x <- case bounds.state of
                                                                Released -> -1
                                                                _ -> bounds.start_x
                                                        , start_y <- case bounds.state of
                                                                Released -> -1
                                                                _ -> bounds.start_y
                                                        , cur_x <- case bounds.state of
                                                                Released -> -1
                                                                _ -> bounds.cur_x
                                                        , cur_y <- case bounds.state of
                                                                Released -> -1
                                                                _ -> bounds.cur_y
                                                        ,state  <- iState}

drawBoundingRect aShape = 
                      let
                        (dx, dy) = decode aShape
                        cw = abs <| aShape.cur_x - aShape.start_y
                        ch = abs <| aShape.cur_y - aShape.start_y
                        s = rect (toFloat cw)
                          (toFloat ch)
                        |> filled blue
                        |> move ((toFloat dx), (toFloat dy))
                      in
                        s

decode aShape = (aShape.cur_x, aShape.cur_y)

drawRect (aShape,aList) = 
                let 
                      s = drawBoundingRect aShape
                      h = drawHistory aList
                    in 
                      layers [collage collage_width collage_height ([s]++h), message, asText aShape]
drawHistory aList = map drawBoundingRect aList
scaleMousePosition (x,y) = (truncate <| (100 * (toFloat x)/collage_width), truncate <| (100*(toFloat y)/collage_height))
main = let 
            currentDragState = foldp updateDragState dragState Mouse.isDown
            bounds = foldp updateDrawingBounds drawing_bounds(lift2 (,) currentDragState (lift scaleMousePosition Mouse.position))
            shapeList = foldp updateShapes shapes (lift2 (,) bounds currentDragState)            
       in            
            drawRect <~ (lift2 (,) bounds shapeList)
            

message = [markdown|
A rubber banding example: drag the mouse anywhere on the screen.
|]