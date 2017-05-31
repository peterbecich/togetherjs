{-# LANGUAGE OverloadedStrings #-}

module Hub where

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
import           Control.Monad
import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Ord
import           Data.Eq
import           Data.List.Split
import           Data.Maybe
import           System.Random


data Client = Client { clientId :: Text, clientConn :: Connection }

instance Ord Client where
  compare client1 client2 = compare (clientId client1) (clientId client2)

instance Eq Client where
  client1 == client2 = (clientId client1) == (clientId client2)

type Room = Set Client
type Rooms = Map Text Room

addToRoom :: Text -> Client -> Rooms -> Rooms
addToRoom roomName client rooms =
  M.adjust (S.insert client) roomName rooms

-- need to close socket connection?
removeFromRoom :: Text -> Client -> Rooms -> Rooms
removeFromRoom roomName client rooms =
  M.adjust (S.filter (\client' -> (clientId client') /= (clientId client))) roomName rooms

createRoom :: Text -> Rooms -> Rooms
createRoom roomName rooms
  | M.member roomName rooms = rooms
  | otherwise = M.insert roomName S.empty rooms

-- need to close socket connections?
removeRoom :: Text -> Rooms -> Rooms
removeRoom roomName = M.filterWithKey (\roomName' _ -> roomName == roomName')

msgClient :: Text -> Client -> IO ()
msgClient msg client = WS.sendTextData (clientConn client) msg

broadcastToRoom' :: Text -> Room -> IO ()
broadcastToRoom' msg = mapM_ (msgClient msg)

broadcastToRoom :: Text -> Text -> Rooms -> IO ()
broadcastToRoom roomName msg rooms
  | M.member roomName rooms = broadcastToRoom' msg (rooms M.! roomName)
  | otherwise = return ()

fooRoom = extractRoomNameId "foo___atuhs41124"

extractRoomNameId :: Text -> Maybe (Text, Text)
extractRoomNameId msg
  | length spt >= 2 = Just (head spt, last spt)
  | otherwise = Nothing
  where
    splitOn' :: Text -> Text -> [Text]
    splitOn' t1 t2 = T.pack <$> splitOn (T.unpack t1) (T.unpack t2)
    --splitOn' = liftA2 splitOn T.unpack T.unpack
    spt = splitOn' ("_" :: Text) msg

-- https://codereview.stackexchange.com/questions/106443/basic-random-password-generator
randomChar :: IO Char
randomChar = do
  index <- randomRIO (0, length alphabet - 1)
  return $ alphabet !! index
  where alphabet = ['A'..'Z']++['a'..'z']++['0'..'9']

generateRandomRoomId :: IO Text
generateRandomRoomId = do
  chars <- replicateM 32 randomChar
  return $ T.pack chars

-- hubConnect :: MVar Rooms -> PendingConnection -> IO ()
hubConnect :: MVar Rooms -> WS.ServerApp
hubConnect mRooms pending = do
  conn <- WS.acceptRequest pending
  _ <- WS.forkPingThread conn 30
  msg <- WS.receiveData conn :: IO Text
  rooms <- readMVar mRooms
  let maybeRoomName = extractRoomNameId msg
      client = Client "foo" 
  case maybeRoomName of
    Just (roomName, _) -> undefined
    Nothing -> WS.sendTextData conn ("Invalid room name" :: Text)

-- maintain :: WS.Connection -> MVar Rooms -> Client -> IO ()
-- maintain 

