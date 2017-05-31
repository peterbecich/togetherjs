module Example2 where

import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import qualified Web.Scotty as S
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets as WS
import           Network.WebSockets.Connection (defaultConnectionOptions)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Control.Monad (forM_)
import           Control.Concurrent
import           Control.Monad
import           Control.Exception
import           Data.Monoid
import           Data.Char


import qualified Example as Example

--            PendingConnection -> IO ()
helloWSApp :: ServerApp
helloWSApp pendingConn = do
  conn <- acceptRequest pendingConn
  sendTextData conn ("hello from the server!" :: Text)
  

wsApp :: IO Application
wsApp = do
  nonWSApp <- Example.app
  let
    wsApp' :: Application
    wsApp' = websocketsOr defaultConnectionOptions helloWSApp nonWSApp
  pure wsApp'

runWSApp :: IO ()
runWSApp = do
  wsa <- wsApp
  W.run 8081 wsa
