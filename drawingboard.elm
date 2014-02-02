{-- Drawing board  --}
{-- The objective of this board is to present a user with a list of selections 
that can be rubberbandable.
--}
import Mouse
import Window

shapes = []
data DragState = Started | Released
clearGrey = rgba 111 111 111 0.6
dragState = Released
drawing_bounds = {state = Released, start_x = -1, start_y = -1, cur_x = -1, cur_y = -1}
areEqual s1 s2 = (s1.start_x == s2.start_x && s1.start_y == s2.start_y 
                    && s1.cur_x == s2.cur_x && s1.cur_y == s2.cur_y)
isValid s = s.start_x > 0 || s.start_y > 0 || s.cur_x > 0 || s.cur_y > 0

contains (x,y) aShape = aShape.start_x <= x &&  x <= aShape.cur_x
                        && aShape.start_y <= y && y <= aShape.cur_y

delete aShape aList = filter (\iShape -> not (areEqual aShape iShape)) aList
                        
queryShape (x,y) aList = 
            let 
                filteredList = filter (\aShape -> contains (x,y) aShape) aList
            in 
                case filteredList of
                    [] -> drawing_bounds
                    h::t -> h
                    _ -> drawing_bounds
updateShapes (s, aState) aList = 
    case aState of
    Released -> case aList of 
                    [] -> if | isValid s -> [s]
                             | otherwise -> []
                    h::t -> if 
                            | (not (areEqual h s)) && (isValid s) -> s :: aList
                            | otherwise -> aList
    Started -> aList
                               
                                          
                                             
updateDragState isDown aState = if | isDown == True -> 
                                     case aState of
                                        Released -> Started
                                        Started -> Started
                                   | not isDown -> 
                                     case aState of
                                        Started -> Released
                                        Released -> Released
                                        
updateDrawingBounds (iState, (x,y)) bounds = case iState of
                              Started -> {bounds | 
                                            start_x <- case bounds.state of
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

drawBoundingRect (width, height) aShape = 
                      let
                        (dx, dy) = translate (width, height) aShape
                        cw = abs <| aShape.cur_x - aShape.start_y
                        ch = abs <| aShape.cur_y - aShape.start_y
                        s = rect (toFloat cw) (toFloat ch)
                        |> filled clearGrey
                        |> move (dx, dy)
                      in
                        s

makeActiveSelection (width, height) aShape =  
                let s = drawBoundingRect (width, height) aShape in
                s
                    
translate (width, height) aShape = ((toFloat aShape.cur_x) - (toFloat width / 2), (toFloat height / 2 - (toFloat aShape.cur_y)))

drawRect (width, height) (aShape,aList) = 
                let 
                      s = drawBoundingRect (width, height) aShape
                      hist = drawHistory (width, height) aList
                    in 
                      layers [collage width height ([s] ++ hist), message]
drawHistory (width, height) aList = map (\s -> drawBoundingRect (width, height) s) aList


main = let 
            currentDragState = foldp updateDragState dragState Mouse.isDown
            selectedShape = lift (\s -> queryShape s shapes) Mouse.position
            bounds = foldp updateDrawingBounds  drawing_bounds (lift2 (,) 
                    currentDragState Mouse.position)
            shapeList = foldp updateShapes shapes (lift2 (,) bounds currentDragState)            
       in            
            lift2 drawRect Window.dimensions (lift2 (,) bounds shapeList)
            

message = [markdown|
A rubber banding example: drag the mouse anywhere on the screen.
|]