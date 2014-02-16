{-- Diagramming editor --}

import Mouse
import Window
import Keyboard
import String
import Char
clearGrey = rgba 111 111 111 0.6
bordered clr = outlined (solid clr)
{-- Features --}
{-- Maintain a dictionary of shapes --}
{-- Enable keyboard interaction to add text to a shape --}
{-- Color selection widget --}
{-- Line selection widget --}
{-- Actively selected shape can me moved, resized etc. --}
{-- Mouseover on a shape should display a popup text --}
{-- Shapes can be connected --}
{-- Shapes should get saved using autosave --}
{-- FRP approach delays the actual composition to the main function. I need to follow the same
style. Using point free style is also nice to read.
One of the reasons for choosing this application is to try to reproduce commonly occuring widgets
and structure the code in a manner that is maintainable. Here maintainable is defined roughly as follows:
Ref: http://blog.reactiveprogramming.org/wp-uploads/2013/12/FRP4.pdf
Features a and b are compositional if the complexity of  A + B is 
the sum of complexities of A and B
A drawing application has many features that users tend to expect to even cross 
a minimal threshold of usability therefore the core feature list is itself quite 
long, and serves as a good example of testing the composing a functional 
reactive program.
In the current file or any of the elm examples on the site, the 
mainSignal is an example of composition of different signals and the render 
function displays the model. So when we add new features we need to expect
a high level update function to update the state and another to render the new feature.--}




initialText = ""

type DrawingRect = {start_x : Int, start_y: Int, end_x : Int, end_y: Int, text : String}
type DiagramState = {user: String, prev_state : 
                     DrawingState, 
                     state : DrawingState, current_rect : DrawingRect, history : [DrawingRect]
                    ,selectionHistory : [DrawingRect]
                    , selectedShape : DrawingRect
                    , selectionColor : Color
                    ,clickLocation : (Int, Int)
                    , charPressed : Keyboard.KeyCode
                    , keysDown : [Keyboard.KeyCode]
                    , currentText : String}

diagramState = {user = "No user",dimensions = (-1, -1), 
                prevState = Released, state = Released , 
                current_rect = initialRect, history = [],
                selectionHistory = [],
                selectedShape = initialRect,
                selectionColor = blue,
                clickLocation = (-1, -1),
                charPressed = -1,
                keysDown = [], 
                currentText = ""}
                                                   
initialRect = {start_x = -1, start_y = -1, end_x = -1, end_y = -1, text = ""}    
sqrtArea {start_x, start_y, end_x, end_y} = (end_x - start_x) * (end_y - start_y)


translate (width,height) aShape = ((toFloat aShape.end_x) - (toFloat width / 2), (toFloat height / 2 - (toFloat aShape.end_y)))
getDimensions aRect = ((abs <| aRect.start_x - aRect.end_x), (abs <| aRect.start_y - aRect.end_y))


isEqual r1 r2 = r1.start_x == r2.start_x && r1.start_y == r2.start_y && r1.end_x == r2.end_x && r1.end_y == r2.end_y    
isValid aRect = (not (aRect == initialRect)) && (not (sqrtArea aRect == 0))
data DrawingState = Started | Released | Selected
contains (x,y) aShape = 
    let
        width = abs <| aShape.end_x - aShape.start_x
        height = abs <| aShape.end_y - aShape.start_y
        (tx, ty) = ((aShape.end_x - width), (aShape.end_y - height))
        (bx, by) = ((aShape.end_x + width), (aShape.end_y + height))
    in
      tx <= x && x <= bx && ty <= y && y <= by
                        
queryShape (x,y) aList =
                let 
                    l = filter (\aShape -> contains (x,y) aShape) aList
                in 
                    case l of
                        [] -> initialRect
                        h :: t -> h


getString keysDown = case keysDown of
                       [] -> ""
                       h::t -> String.fromList [Char.fromCode h]


find aRect aHistory = 
    let
        q = filter (\x  -> isEqual x aRect) aHistory
    in
      case q of
        [] -> initialRect
        h::t-> h



updateDimensions (width, height) aState = {aState | dimensions <- (width, height)}
updateHistory aState = 
    let 
        s = aState.current_rect
        hist = aState.history
        cur = aState.state
        prev = aState.prevState
    in    
        case (prev, cur) of 
          (Released, Released) -> {aState | 
                                history <- case hist of
                                             [] -> if 
                                                 | isValid s -> [s]
                                                 | otherwise -> []
                                             h::t -> if 
                                                    | (not (isEqual h s)) && (isValid s) -> s :: hist
                                                    | otherwise -> hist
                      }
            {-- No op --}
          (Released, Started) -> aState
          (Started, Started) -> aState
          (Started, Released) -> aState
        
        
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

selectShape aState = 
    let selectedShape = aState.selectedShape in
    {aState | selectedShape <- queryShape aState.clickLocation aState.history
    }
{-- Update the text for the currently selected shape --}
{-- There can be only one selected shape --}

replace aText current_rect aHistory = map (\aRect -> 
                                               if | isEqual aRect current_rect -> {aRect| text <- aText}
                                                  |otherwise -> aRect) aHistory
    
updateSelectedText keysDown aState =
    let 
        sShape = aState.current_rect
        newChar = getString keysDown
        ss = aState.selectedShape
        current_rect = aState.current_rect
        history = aState.history
        newText = sShape.text ++ newChar
    in
      {
        aState|current_rect <- {sShape | text <- newText},
                               history <- replace newText current_rect history
      }



    
updateDrawingBounds (x,y) aState = 
    let cur = aState.state
        prev = aState.prevState
        newRect x1 y1 x2 y2 iText = {start_x = x1, start_y = y1, end_x = x2, end_y = y2, text = iText}
        c = aState.current_rect
    in
      {aState | current_rect <- 
                  case (cur, prev) of
                    (Started, Released) -> newRect x y x y ""
                    (Started, Started)-> newRect c.start_x c.start_y x y c.text
                    (Released, Released)-> c
                    (Released, Started) -> c
      }


updateKeysDown keys aState = {aState | keysDown <- keys}

drawRectWithText (width, height) aRect lineStyle color =
    let 
        (dx, dy) = translate (width, height) aRect
        (rW, rH) = getDimensions aRect
        aText = aRect.text
        s = rect (toFloat rW) (toFloat rH)
                              |> lineStyle color
                              |> move (dx, dy)
        shapeText = (toForm <| plainText aText) |> move (dx, dy)       
    in 
      [s, shapeText]

    
drawSelectionRect aState aRect = drawRectWithText aState.dimensions aRect bordered 
                                 aState.selectionColor
    
drawBoundingRect aState aRect = drawRectWithText aState.dimensions aRect filled clearGrey


drawHistory aState = 
    let 
        hist = aState.history
        dimensions = aState.dimensions
    in
      map (\s -> drawBoundingRect aState s) hist

inputSignal = lift5 (,,,,) Mouse.isDown Mouse.position Window.dimensions 
              (sampleOn Mouse.clicks Mouse.position) Keyboard.keysDown
handle (isDown, (x,y), (width, height),location, keysDown) = updateDrawingBounds (x,y) . 
                                    updateDragState isDown .
                                    updateDimensions (width, height) .
                                    updateHistory .
                                    updateClickLocation location .
                                    selectShape .
                                    updateSelectedText keysDown .
                                    updateKeysDown keysDown

                                 
render dState = 
    let 
        (width, height) = dState.dimensions
        aRect = drawBoundingRect dState dState.current_rect
        rectHistory = drawHistory dState
        selectedShape = drawSelectionRect dState dState.current_rect
    in
      layers [collage width height <| (aRect ++ (concat rectHistory)), logMessage dState]

mainSignal = foldp handle diagramState inputSignal
main =  render <~ mainSignal
logMessage aState = asText aState
