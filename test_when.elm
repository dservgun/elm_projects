import Mouse
-- This is the approach to implementing rubber banding
-- courtesy ZS from the mailing list.
-- This is the general approach to modeling applications in ELM
-- store the state using a fold function: that is the cleanest 
-- way to maintain memory

initial = {down = False, start_x = -1}
updateState (mouseDown, (xpos,_)) state =
  {state | down <- mouseDown,
           start_x <- if | state.down -> xpos
                         | otherwise -> -1}

main = asText <~ foldp updateState initial (lift2 (,) Mouse.isDown Mouse.position)