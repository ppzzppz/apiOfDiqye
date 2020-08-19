{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.AppM
import Web.Static.Static
import Web.WebSocket.WebSocket
import Control.Applicative( (<|>) )
import Control.Exception (displayException)
import System.Environment(lookupEnv)
import System.Directory(getHomeDirectory)
import Data.Text.Lazy(Text)
import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.Exception



myapp :: AppIO
myapp = msum
  [ consum "share-text" >> respSocket' shareText
  , consum "share" >> share
  ]

type ShareData = (Text,[(Integer,Connection)],Integer)
shareData :: MVar ShareData
shareData = unsafePerformIO $ newMVar ("[0,\"Welcome\",0]",[],0)

shareText :: ServerApp
shareText pending_conn = do
  conn <-  acceptRequest pending_conn
  (initMsg,conns,n) <- takeMVar shareData
  -- withPingThread conn 30 (pure ()) (pure ())
  sendTextData conn initMsg
  putMVar shareData (initMsg,(n,conn):conns,n+1)
  forever $ do
    msg <- receiveData conn 
    modifyMVar_ shareData $ \(_,b,n) -> pure (msg,b,n)
    sendAll n

sendAll :: Integer -> IO ()
sendAll nsent = do
  (msg,conns,n) <- takeMVar shareData
  let meList = filter ((==nsent) . fst) conns
  let peers = filter ((/=nsent) . fst) conns
  successPeers <- sendMsgFor msg peers
  me <- sendOkForMe meList
  putMVar shareData (msg,me ++ successPeers,n)


sendOkForMe ::  [(Integer,Connection)] -> IO [(Integer, Connection)]
sendOkForMe [] = pure []
sendOkForMe (peer@(_,conn):_) = do
  success <- (True <$ sendTextData conn ("[700]"::Text)) `catch` errorHandle
  if success then pure [peer] else pure []

sendMsgFor :: Text -> [(Integer, Connection)] -> IO [(Integer, Connection)]
sendMsgFor msg [] = pure []
sendMsgFor msg (peer@(_,conn):xs) = do
  success <- (True <$ sendTextData conn msg) `catch` errorHandle
  if success then pure (:) <*> pure peer <*> sendMsgFor msg xs else sendMsgFor msg xs

errorHandle :: ConnectionException -> IO Bool
errorHandle _ = pure False  

share :: AppIO
share = do
  folder <- shareFolder
  (dirServe folder ["index.html"] <|> dirBrowse folder)

shareFolder :: (Monad m,MonadIO m) => m String
shareFolder =  liftIO $ do
  folder <- lookupEnv "share_folder"
  home <- getHomeDirectory
  pure $ maybe home id folder 


setting = setPort 8899
  $ setOnException (\ _ e -> putStrLn $ ("**OnException:\n" ++) $ displayException e)
  $ setOnExceptionResponse exceptionResponseForDebug
  $ setTimeout (30*60*60)
  $ defaultSettings

main :: IO ()
main = runSettings setting $ toApplication $ myapp