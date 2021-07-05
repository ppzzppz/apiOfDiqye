{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Chat where 
import Web.AppM
import Web.WebSocket.WebSocket
import Control.Applicative
import Control.Concurrent.MVar
import GHC.Generics
import Control.Exception(catch)
import System.IO.Unsafe
import Data.Aeson
import Data.Either
import Data.Time.Clock.POSIX (getPOSIXTime)


type PeerInfo = String
data Peer = Peer String Connection PeerInfo

data MItem = TextM String | ImageM String
  deriving (Show,Eq,Generic)

data Message = Message {
  time :: Integer,
  message :: MItem,
  sender :: (String,String) -- name key
} deriving (Show,Eq,Generic)

-- | empty server send
-- | 
type ReqId = String


type PassData a = (ReqId,a)

data Req = EEE |
  Send MItem 
  deriving (Show,Eq,Generic)
data Res = 
  Init [Message] | 
  Fail String |
  SendOk |
  OneMessage Message
  deriving (Show,Eq,Generic)

type RoomInfo = [Message]
emptyRoomInfo = []
newtype Room = Room (String,[Peer],RoomInfo)

instance Eq Room where
  (Room (room1,_,_)) == (Room (room2,_,_)) = room1 == room2

instance Eq Peer where
  Peer key1 _ _ == Peer key2 _ _ =  key1 == key2



type ChatPool = [Room]

newtype Kickout a = Kickout a
  deriving (Generic,Show)

instance ToJSON a => ToJSON (Kickout a)
instance ToJSON MItem
instance ToJSON Message
instance FromJSON MItem
instance FromJSON Message
instance ToJSON Req
instance FromJSON Req
instance ToJSON Res
instance FromJSON Res


chatPool ::  MVar ChatPool
chatPool = unsafePerformIO $ newMVar []

roomTuple room@(Room (name,_,_)) = (name,room)

lookupRoom :: String -> ChatPool -> Maybe Room
lookupRoom room = lookup room . map roomTuple


kickout :: String -> String -> IO ()
kickout roomName key = do
  modifyMVar_ chatPool $ \ pool -> do
    npool <- runMaybeT $ kickoutFromPool (roomName,key) pool
    pure $  maybe pool id npool

putRoomAPeer :: Peer -> Room -> Room
putRoomAPeer peer (Room (name,peers,info)) = Room (name,peer:peers,info)

replaceRoomFromPool :: Room -> ChatPool -> ChatPool
replaceRoomFromPool room pool = map (replace room) pool
  where replace room oldroom | room == oldroom = room
                             | otherwise = oldroom

kickoutFromPool :: (String,String) -> ChatPool -> MaybeT IO ChatPool
kickoutFromPool (roomName,key) pool = do
  room@(Room (_,peers,info)) <-  MaybeT $ pure $ lookupRoom roomName pool
  peer@(Peer _ conn _) <- MaybeT $ pure $ lookup key $ map (\ peer@(Peer key _ _)->(key,peer)) peers
  liftIO $ sendClose conn $ encode $ Kickout (roomName,key)
  let newPeers = filter (/=peer) peers
  let newPool = if null newPeers then filter (/=room) pool else map (instead (Room (roomName,newPeers,info))) pool
  pure newPool
  where 
    instead :: Room -> Room -> Room
    instead newRoom oldRoom = if newRoom == oldRoom then newRoom else oldRoom


chatApp :: AppIO
chatApp = do
  room <- consumV <|> pure "public"
  name <- queryV "name"
  key <- queryV "key"
  respSocket' $ chatSocket (key,name,room) 

removeConn :: String -> String -> IO ()
removeConn roomName key = do
  modifyMVar_ chatPool $ \ pool -> do
    pure $ map fn' pool
  where
    fn' room@(Room (name,peers,c)) | name == roomName = Room (name,filter fn1 peers,c)
                                   | otherwise = room
    fn1 (Peer peerkey _ _) | key == peerkey = False
                           | otherwise = True


putConn :: String -> String -> String -> Connection -> IO ()
putConn roomName key peerName conn = do
  modifyMVar_ chatPool $ \ pool -> do
    (Just npool) <- runMaybeT $ inaroom pool <|> newroom pool
    pure npool
  where 
    inaroom :: ChatPool -> MaybeT IO ChatPool
    inaroom pool = do
      room <-  MaybeT $ pure $ lookupRoom roomName pool 
      let nroom = putRoomAPeer (Peer key conn peerName) room
      pure $ replaceRoomFromPool nroom pool

    newroom :: ChatPool ->  MaybeT IO ChatPool
    newroom pool = do
      pure $ Room (roomName,[Peer key conn peerName],[]) : pool

reciveMessage :: (String,String) -> Connection -> IO ()
reciveMessage (roomName,peerKey) conn = do
  msg <- receiveData conn
  let msgEigher = eitherDecode msg :: Either String (PassData Req)
  msg' <- do
    case msgEigher of Left s -> sendOne conn ("",(Fail s)) >> (mempty :: IO ()) >> pure ("",EEE)
                      Right a -> pure a
  let room' = Room (roomName,[],emptyRoomInfo)
  let sender' = (roomName,peerKey) 
  modifyMVar_ chatPool $ \ pool -> do
    mapM (\ room -> if room == room' then  sendRoom msg' room sender' else pure room)  pool

sendRoom :: PassData Req -> Room -> (String,String) -> IO Room
sendRoom (reqId, Send item) (Room (name,peers',info)) senderOne = do
  let peerName = maybe "" id $ lookup (snd senderOne) (map (\ (Peer key _ name) -> (key,name)) peers')
  time <-  (round . (*1000)) <$> getPOSIXTime
  let message = Message {time = time,message=item,sender=(peerName,snd senderOne)}
  peers <- send' peers' message
  pure $ Room (name,peers,take 100 $ message:info)
  where
    send' [] _ = pure []
    send' (peer@(Peer key conn _):tail) message = do
      let (_,key') = sender message
      if key == key' 
      then 
        sendOne conn (reqId,SendOk) >> ((peer:) <$> send' tail message)
         `catch` ignoreErrorThen (send' tail message)
      else
        sendOne conn (reqId,OneMessage message) >> ((peer:) <$> send' tail message)
        `catch` ignoreErrorThen (send' tail message)
    
ignoreErrorThen :: IO [Peer] -> ConnectionException -> IO [Peer]
ignoreErrorThen peers _ = peers

sendOne :: ToJSON a => Connection -> PassData a -> IO ()
sendOne conn rdata = do
  sendTextData conn $ encode rdata

chatSocket :: (String,String,String) -> ServerApp
chatSocket (key,name,room) pending_conn = do
  kickout room key
  conn <-  acceptRequest pending_conn
  putConn room key name conn
  pool <- readMVar chatPool
  runMaybeT $ do
    (Room (_,_,roomInfo)) <-  MaybeT $ pure $ lookupRoom name pool 
    liftIO $ sendOne conn $ ("",Init roomInfo)
  (forever $ reciveMessage (room,key) conn)
    `catch` errorHandle' room key

errorHandle' :: String -> String -> ConnectionException  -> IO ()
errorHandle' room key e = do
  removeConn room key