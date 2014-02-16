{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.WebSockets as WS
import System.Environment(getEnv)

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
     TIO.putStrLn(msg `T.append` ", meow")
     WS.sendTextData conn $ msg `T.append` ", woof"
