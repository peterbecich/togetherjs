{-# LANGUAGE OverloadedStrings #-}

module Chat where

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
import           Control.Concurrent
import           Control.Monad
import           Control.Exception
import           Data.Monoid
import           Data.Char


import qualified Example as Example

-- https://github.com/jaspervdj/websockets/blob/master/example/server.lhs
type Client = (Text, WS.Connection)
type ServerState = [Client]

initialServerState :: ServerState
initialServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client state
  | clientExists client state = state
  | otherwise = client : state

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  _ <- T.putStrLn message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

--               MVar ServerState -> PendingConnection -> IO ()
chatServerApp :: MVar ServerState -> WS.ServerApp
chatServerApp state pending = do
  conn <- WS.acceptRequest pending
  _ <- putStrLn "New connection"
  _ <- WS.forkPingThread conn 5
  msg <- WS.receiveData conn
  _ <- putStrLn $ "New message received from client: "++ show msg
  clients <- readMVar state
  _ <- putStrLn $ "num clients: " ++ (show (numClients clients))
  let client = getClient conn msg
  _ <- WS.sendTextData conn ("Welcome" :: Text)
  case msg of
    _ | not (T.isPrefixOf prefix msg) -> do
          _ <- putStrLn "wrong announcement"
          WS.sendTextData conn ("Wrong announcement" :: Text)
      | any ($ fst client) [T.null, T.any isPunctuation, T.any isSpace] -> do
          _ <- putStrLn "Invalid name"
          WS.sendTextData conn ("Name cannot " <>
                                " contain punctuation or whitespace, and " <>
                                " cannot be empty" :: Text )
      | clientExists client clients -> do
          _ <- putStrLn "User already exists"
          WS.sendTextData conn ("User already exists" :: Text)
      | otherwise -> flip finally (disconnect conn msg) $ do
          _ <- modifyMVar_ state $ \s -> do
            let s' = addClient client s
            _ <- WS.sendTextData conn $ "Welcome! Users: " <> T.intercalate ", " (map fst s)
            _ <- broadcast (fst client <> " joined") s'
            return s'
          _ <- putStrLn "'chatServerApp' gives control to 'talk'"
          _ <- talk conn state client
          putStrLn "'talk' returns control to 'chatServerApp'"
          
  where
    prefix = "Hi! I am "
    getClient conn msg = (T.drop (T.length prefix) msg, conn) -- (Text, Connection)
    disconnect conn msg = do
      s <- modifyMVar state $ \s ->
        let s' = removeClient (getClient conn msg) s in return (s', s')
      broadcast (fst (getClient conn msg) <> " disconnected") s
                                            
removeClient :: Client -> ServerState -> ServerState
removeClient client state = filter (\(client', _) -> fst client /= client') state

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (user, _) = forever $ do
  _ <- putStrLn $ "talk user: " <> (show user)
  msg <- WS.receiveData conn
  _ <- putStrLn $ "talk user: " <> (show user) <> " msg: " <> (show msg)
  state' <- readMVar state
  broadcast (user <> ": " <> msg) state'

chatApp :: IO Application
chatApp = do
  nonWSApp <- Example.app
  initialMVar <- newMVar initialServerState
  let
    wsApp' :: Application
    wsApp' = websocketsOr defaultConnectionOptions (chatServerApp initialMVar) nonWSApp
  pure wsApp'

runChatApp :: IO ()
runChatApp = do
  chat <- chatApp
  W.run 8081 chat
  
