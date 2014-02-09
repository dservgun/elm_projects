import Mouse
import Window

clearGrey = rgba 111 111 111 0.6

translate (x, y) (width, height) = 
  (toFloat x - toFloat width /2, toFloat height /2 - toFloat y)
render (x,y) (width, height)= 
      collage width height
       [rect (toFloat 200) (toFloat 300) |> filled clearGrey |> 
         move (translate (x, y) (width, height))]
main = lift2 render Mouse.position Window.dimensions 
