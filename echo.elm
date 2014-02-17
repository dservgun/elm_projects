import Char
import Maybe
import Http
import Graphics.Input as Input
import WebSocket

defaultUrl = "ws://localhost:8082"

type UIState = {requests : [String], 
                responses : [String]}
initState : UIState
initState = {requests = [],
             responses = []}

incoming : Signal String
incoming = WebSocket.connect defaultUrl outgoing

outgoing : Signal String
outgoing = content

(field, content) = Input.field "Echo"
drawString aString = plainText <| "" ++ aString
outgoingMessages = WebSocket.connect defaultUrl content

     

main =  lift2 above field (lift drawString incoming)
