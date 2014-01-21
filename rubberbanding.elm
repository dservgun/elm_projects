import Http
import Json
import Mouse
import Window



isEven n = n `mod` 2 == 0
clearGrey = rgba 111 111 111 0.6
mouseDown = keepWhen Mouse.isDown (0,0) Mouse.position
mouseDownLocations = foldp (::) [] (mouseDown)
mouseDownOrigin = foldp (\n aList -> 
                            case aList of
                             [] -> n :: []
                             _ -> aList) [] mouseDown

textDraw origin destination = 
  case origin of
  [x] -> (x, destination)
  _ -> (destination, destination)


main = lift2 scene2 mouseDownOrigin Mouse.position

decode origin  (r1, r2)=
    case origin of
    [(x,y)] -> (toFloat x, toFloat y)
    _ -> (toFloat r1, toFloat r2)
    
scene2 origin (w,h) =
        let 
            (dx, dy) = decode origin (w,h)
        in  
            collage w h
           [ rect (toFloat w) (toFloat h) |> filled blue |> move (dx, dy)
           ]


