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



main = scene <~ mouseDownOrigin ~ Mouse.position



-- DISPLAY    
scene origin (w,h) =
        let 
            (dx, dy) = decode origin (w,h) 
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

decode origin  (r1, r2) =
    case origin of
    [(x,y)] -> ((toFloat x), -1 * toFloat y )
    _ -> (toFloat r1, toFloat r2)
           
isEven n = n `mod` 2 == 0
clearGrey = rgba 111 111 111 0.6
message = [markdown|
A rubber banding example
|]