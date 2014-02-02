{-- Diagramming editor --}
{-- The objective is to design a diagramming editor 
using elm.--}

import Mouse
import Window
import Keyboard

{-- Features --}
{-- Rubber band a currently selected shape --}
{-- Refactor the drawingboard.elm code --}
{-- Maintain a dictionary of shapes --}
{-- Enable keyboard interaction to add text to a shape --}
{-- Color selection widget --}
{-- Line selection widget --}
{-- Actively selected shape can me moved, resized etc. --}
{-- Mouseover on a shape should display a popup text --}
{-- Shapes can be connected --}
{-- FRP delays the actual composition to the main function. I need to follow the same
style. Using point free style is also nice to read. --}

data DrawingState = NewDrawingStarted | NewDrawingReleased | Selected
type DrawingRect = {start_x : Int, start_y: Int, end_x : Int, end_y: Int}
type DiagramState = {user: String, prev_state : DrawingState, state : DrawingState, current_rect : DrawingRect, history : [DrawingRect]}
initialState = {user = "No user", prevState = NewDrawingReleased, state = NewDrawingReleased , 
    current_rect = initialRect, history = []}

initialRect = {start_x = -1, start_y = -1, end_x = -1, end_y = -1}    
isValid aRect = not (aRect == initialRect) 
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

-- All functions need to update the state so that we 
-- can invoke a foldp and use the point free style                        
updateShapes aState = 
    let 
        s = aState.current_rect 
        hist = aState.history
    in    
        case aState.state of 
            NewDrawingReleased -> {aState | history <- case hist of
                                  [] -> if | isValid s -> [s]
                                           | otherwise -> []
                                  h::t -> if 
                                            | (not (h == s)) && (isValid s) -> s :: hist
                                            | otherwise -> hist
                                  }
            -- No op
            NewDrawingStarted -> aState
        
        
updateDragState isDown aState = 
    {aState | 
             prevState <- aState.state
            ,state <-
    if | isDown ->
            case aState.state of
                NewDrawingReleased -> NewDrawingStarted
                NewDrawingStarted -> NewDrawingStarted
        |otherwise -> 
            case aState.state of
                NewDrawingReleased -> NewDrawingReleased
                NewDrawingStarted -> NewDrawingReleased
    }


main =  asText <~ Mouse.position
