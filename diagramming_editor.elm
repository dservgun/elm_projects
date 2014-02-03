{-- Diagramming editor --}



import Mouse
import Window
import Keyboard
clearGrey = rgba 111 111 111 0.6
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
                     state : DrawingState, current_rect : DrawingRect, history : [DrawingRect]}

diagramState = {user = "No user",dimensions = (-1, -1), 
                prevState = Released, state = Released , 
                current_rect = initialRect, history = []}
                                                   
initialRect = {start_x = -1, start_y = -1, end_x = -1, end_y = -1}    
isValid aRect = not (aRect == initialRect) 
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
                        _ -> initialRect -- There should be no duplicates. We need to maintain a dictionary


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


inputSignal = lift3 (,,) Mouse.isDown Mouse.position Window.dimensions
handle (isDown, (x,y), (width, height)) = updateDrawingBounds (x,y) . 
                                    updateDragState isDown .
                                    updateDimensions (width, height) .
                                    updateHistory

render dState = 
    let 
        (width, height) = dState.dimensions
        aRect = drawBoundingRect dState dState.current_rect
        rectHistory = drawHistory dState
    in
      layers [collage width height <| [aRect] ++ rectHistory]

mainSignal = foldp handle diagramState inputSignal
main =  render <~ mainSignal        