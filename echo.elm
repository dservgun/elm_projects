import Char
import String
import Maybe
import Http
import Graphics.Input as Input
import WebSocket

defaultUrl = "ws://localhost:8082"
(field, content) = Input.field "Echo"
drawString aString = plainText <| "" ++ aString
outgoingMessages = WebSocket.connect defaultUrl content
main =  lift2 above field (lift drawString outgoingMessages)
