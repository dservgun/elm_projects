{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative ( (<$>) )
import Control.Monad (forever)
import Control.Exception(bracket)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.WebSockets as WS
import System.Environment(getEnv)
import Data.Acid
import Data.Acid.Remote
import Data.SafeCopy
import Data.Typeable
import qualified Data.Map as Map

type Email = String
type Name = String

data AddressBook = AddressBook ! (Map.Map Email Name)
     deriving (Typeable)

$(deriveSafeCopy 0 'base ''AddressBook)

  
insertEmail :: Email -> Name -> Update AddressBook ()  
insertEmail email aName
  = do AddressBook a <- get
       put (AddressBook (Map.insert email aName a))

lookupEmail :: Email -> Query AddressBook (Maybe Name)
lookupEmail email =
  do AddressBook a <- ask
     return (Map.lookup email a)

viewMessages :: Int  -> Query AddressBook [Email]
viewMessages aNumber =
  do AddressBook a <- ask
     return $ take aNumber (Map.keys a)
     
$(makeAcidic ''AddressBook ['insertEmail, 'lookupEmail, 'viewMessages])

main :: IO ()
main = 
  bracket
  (openLocalState $ AddressBook Map.empty)
  closeAcidState
  (\acid -> WS.runServer "127.0.0.1" 8082 $ handleConnection acid)

{--
A simple echo.
--}

handleConnection acid pending = do
  os <- getEnv("os")
  putStrLn("Starting server on. - ." ++ os)
  conn <- WS.acceptRequest pending
  putStrLn("Accepted connection")
  sendHistory conn acid
  echo conn acid


sendHistory conn acid =
  do
    messages <- query acid (ViewMessages 100)
    mapM_ (\m -> WS.sendTextData conn (T.pack m)) messages

echo conn acid = 
  forever $ do
     msg <- WS.receiveData conn
     TIO.putStrLn(msg)
     update acid $ InsertEmail (T.unpack msg)  (T.unpack msg)
     WS.sendTextData conn msg
