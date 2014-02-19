{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative ( (<$>) )
import Control.Monad (forever)
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

$(makeAcidic ''AddressBook ['insertEmail, 'lookupEmail])

main :: IO ()
main = do
  test <- getEnv "os"
  putStrLn("Starting server on. - ." ++ test)
  WS.runServer "127.0.0.1" 8082 $ handleConnection

{--
A simple echo.
--}

handleConnection pending = do
  conn <- WS.acceptRequest pending
  putStrLn("Accepted connection")
  echo conn


echo conn = 
  forever $ do
     msg <- WS.receiveData conn
     TIO.putStrLn(msg)
     WS.sendTextData conn msg
