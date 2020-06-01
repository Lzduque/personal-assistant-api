module Main where

import Web.Scotty (scotty, get, json, html)
import Control.Monad.IO.Class (liftIO)

import Lib
import Greeting (greeting)

main :: IO ()
main = do
  scotty 3000 $ do
    get "/greeting" $ do
        liftIO $ putStrLn "HI!!!!"
        -- html "This was a GET request!"  -- send 'text/html' response
        greet <- liftIO $ greeting
        json greet
