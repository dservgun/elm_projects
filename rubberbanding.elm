import Http
import Json
import Mouse
import Window

{--
Rubber banding example. 
For the example to work correctly, we need another signal: clearing or
getting out of the draw mode.
The basic tutorial talks about the following structure of each elm application:
Model, Update and Display.

--}

-- UPDATE
mouseDown = keepWhen Mouse.isDown (0,0) Mouse.position
mouseDownOrigin = foldp (\n aList -> 
                            case aList of
                             [] -> n :: []
                             _ -> aList) [] mouseDown



  
main = lift3 scene mouseDownOrigin Mouse.position Window.height


-- DISPLAY    
scene origin (w,h) windowHeight=
        let 
            (dx, dy) = decode origin (w,h) windowHeight
            c = collage w h
                [ rect (toFloat w) (toFloat h) |> filled blue |> move (dx, dy)
                ]
        in layers [c, message]
-- UTILITIES
-- For debugging.
textDraw origin destination = 
  case origin of
  [x] -> (x, destination)
  _ -> (destination, destination)

decode origin  (r1, r2) windowHeight=
    case origin of
    [(x,y)] -> ((toFloat x), toFloat y  - (toFloat windowHeight / 2))
    _ -> (toFloat r1, toFloat r2)
           
isEven n = n `mod` 2 == 0
clearGrey = rgba 111 111 111 0.6
message = [markdown|
A rubber banding example
|]