import Mouse
import Keyboard
import Window

clearGrey = rgba 111 111 111 0.6
bordered clr = outlined <| solid clr

data SelectionMode = Rectangle | Pointer | Circle

type Shape = {origin : (Int, Int), width : Int, height : Int}
type Selector ={shape: Shape, url : String}
-- List of shapes in the flow chart
type Flowchart ={user: String,
                 dimensions : (Int, Int),
                 currentPosition : (Int, Int),
                 selection : SelectionMode,
                 selectorDatabase :[Selector],
                 database: [Shape],
                 clickLocation : (Int, Int)
                }

flowchart = {user = "No user",
             selection = Rectangle,
             selectorDatabase =[],
             currentPosition = (-1,-1),
             dimensions = (-1, -1),
             clickLocation = (-1, -1),
             database = []}


(defaultWidth, defaultHeight) = (100, 50)
translate (x, y) (width, height) =
    (toFloat x - toFloat width / 2, toFloat height /2 - toFloat y)

imageWidth = 50
imageHeight = 50

selectorImages origin = map(\a -> a) [fittedImage imageWidth imageHeight "/pointer_1.jpg", 
                                          fittedImage imageWidth imageHeight "/square.jpg", 
                                          fittedImage imageWidth imageHeight "/circle.jpg"]

selectorCollage width height = flow right (selectorImages (0, -500))

draw (x,y) (width, height) = 
    collage width height
            [rect (toFloat defaultWidth) 
                  (toFloat defaultHeight)
                  |> filled clearGrey
                  |> move (translate (x, y) (width, height))]
drawShape (width, height) aShape= rect (toFloat aShape.width) (toFloat aShape.height) |> filled clearGrey
    |> move (translate (aShape.origin) (width, height))

drawDatabase dimensions database = map (\r -> drawShape dimensions r) database
inputSignal = lift5 (,,,,) Mouse.isDown Mouse.position Window.dimensions 
              (sampleOn Mouse.clicks Mouse.position) Keyboard.keysDown

handle (isDown, (x,y), (width, height), location, keysDown) =
    updateCurrentPosition (x,y) .
    updateSelectionMode .
    updateShapeDatabase (x,y) .
    updateClickLocation location


updateClickLocation aLoc f ={f | clickLocation <- aLoc}
updateCurrentPosition (x, y) f = {f | currentPosition <- (x,y)}
updateSelectionMode f = {f | selection <- Rectangle}
updateShapeDatabase (x, y) f = {f | database <- {origin = (x,y), height = defaultHeight,
                                                          width = defaultWidth} :: f.database}

render f = 
    let (width, height) = f.dimensions
        shapes = f.database
    in
      layers [collage width height <| 
                          (drawDatabase (width, height) shapes), (selectorCollage width height), asText f]
mainSignal = foldp handle flowchart inputSignal
main = render <~ mainSignal
