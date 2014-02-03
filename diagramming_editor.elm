{-- Diagramming editor --}

import Mouse
import Window
import Keyboard
clearGrey = rgba 111 111 111 0.6
bordered clr = outlined (solid clr)
{-- Features --}
{-- Rubber band a currently selected shape --}
{-- Maintain a dictionary of shapes --}
{-- Enable keyboard interaction to add text to a shape --}
{-- Color selection widget --}
{-- Line selection widget --}
{-- Actively selected shape can me moved, resized etc. --}
{-- Mouseover on a shape should display a popup text --}
{-- Shapes can be connected --}
{-- FRP approach delays the actual composition to the main function. I need to follow the same
style. Using point free style is also nice to read. --}





type DrawingRect = {start_x : Int, start_y: Int, end_x : Int, end_y: Int}
type DiagramState = {user: String, prev_state : 
                     DrawingState, 
                     state : DrawingState, current_rect : DrawingRect, history : [DrawingRect]
                    , selectedShape : DrawingRect
                    , selectionColor : Color
                    ,clickLocation : (Int, Int)}

diagramState = {user = "No user",dimensions = (-1, -1), 
                prevState = Released, state = Released , 
                current_rect = initialRect, history = [],
                selectedShape = initialRect,
                selectionColor = blue,
                clickLocation = (-1, -1)}
                                                   
initialRect = {start_x = -1, start_y = -1, end_x = -1, end_y = -1}    
sqrtArea {start_x, start_y, end_x, end_y} = (end_x - start_x) * (end_y - start_y)
    
isValid aRect = (not (aRect == initialRect)) && (not (sqrtArea aRect == 0))
data DrawingState = Started | Released | Selected
contains (x,y) aShape = aShape.start_x <= x && x <= aShape.end_x
                        && aShape.start_y <= y && y <= aShape.end_y

                        
queryShape (x,y) aList =
                let 
                    l = filter (\aShape -> contains (x,y) aShape) aList
                in 
                    case l of
                        [] -> initialRect
                        h :: t -> h
                        _ -> initialRect


updateDimensions (width, height) aState = {aState | dimensions <- (width, height)}
-- All functions need to update the state so that we 
-- can invoke a foldp and use the point free style                        
updateHistory aState = 
    let 
        s = aState.current_rect 
        hist = aState.history
    in    
        case aState.state of 
            Released -> {aState | history <- case hist of
                                  [] -> if | isValid s -> [s]
                                           | otherwise -> []
                                  h::t -> if 
                                            | (not (h == s)) && (isValid s) -> s :: hist
                                            | otherwise -> hist
                                  }
            -- No op
            Started -> aState
        
        
updateDragState isDown aState = 
    {aState | 
             prevState <- aState.state
            ,state <-
    if  | isDown ->
            case aState.state of
                Released -> Started
                Started -> Started
        | otherwise -> 
            case aState.state of
                Released -> Released
                Started -> Released
    }


updateClickLocation location aState = {aState | clickLocation <- location}                                               
selectShape aState = {aState | selectedShape <- queryShape aState.clickLocation aState.history}


updateDrawingBounds (x,y) aState = 
    let cur = aState.state
        prev = aState.prevState
        newRect x1 y1 x2 y2 = {start_x = x1, start_y = y1, end_x = x2, end_y = y2}
        c = aState.current_rect
    in
      {aState | current_rect <- 
                  case (cur, prev) of
                    (Started, Released) -> newRect x y x y
                    (Started, Started)-> newRect c.start_x c.start_y x y
                    (Released, Released)-> initialRect
                    (Released, Started) -> c
      }

translate (width,height) aShape = ((toFloat aShape.end_x) - (toFloat width / 2), (toFloat height / 2 - (toFloat aShape.end_y)))
getDimensions aRect = ((abs <| aRect.start_x - aRect.end_x), (abs <| aRect.start_y - aRect.end_y))

drawSelectionRect aState aRect =
    let 
        (width, height) = aState.dimensions
        (dx, dy) = translate (width, height) aRect
        (rW, rH) = getDimensions aRect
        s = rect (toFloat rW) (toFloat rH)
                              |> bordered aState.selectionColor
                              |> move (dx, dy)
    in 
      s

    
drawBoundingRect aState aRect =
    let 
        (width, height) = aState.dimensions
        (dx, dy) = translate (width, height) aRect
        (rW, rH) = getDimensions aRect
        s = rect (toFloat rW) (toFloat rH)
                              |> filled clearGrey
                              |> move (dx, dy)
    in 
      s

drawHistory aState = 
    let 
        hist = aState.history
        dimensions = aState.dimensions
    in
      map (\s -> drawBoundingRect aState s) hist


inputSignal = lift4 (,,,) Mouse.isDown Mouse.position Window.dimensions (sampleOn Mouse.clicks Mouse.position)
handle (isDown, (x,y), (width, height),location) = updateDrawingBounds (x,y) . 
                                    updateDragState isDown .
                                    updateDimensions (width, height) .
                                    updateHistory .
                                    updateClickLocation location .
                                    selectShape

render dState = 
    let 
        (width, height) = dState.dimensions
        aRect = drawBoundingRect dState dState.current_rect
        rectHistory = drawHistory dState
        selectedShape = drawSelectionRect dState dState.selectedShape
    in
      layers [collage width height <| [aRect] ++ rectHistory ++ [selectedShape]]

mainSignal = foldp handle diagramState inputSignal
main =  render <~ mainSignal