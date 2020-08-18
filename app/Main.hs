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

type ShareData = (Text,[Connection])
shareData :: MVar ShareData
shareData = unsafePerformIO $ newMVar ("[0,\"Welcome\",0]",[])

shareText :: ServerApp
shareText pending_conn = do
  conn <-  acceptRequest pending_conn
  (initMsg,_) <- readMVar shareData
  -- withPingThread conn 30 (pure ()) (pure ())
  sendTextData conn initMsg
  modifyMVar_ shareData $ \(msg,conns) -> do
    pure (msg,conn:conns)
  forever $ do
    msg <- receiveData conn
    modifyMVar_ shareData $ \(_,b) -> pure (msg,b)
    sendAll

sendAll :: IO ()
sendAll = do
  (msg,conns) <- takeMVar shareData
  newConns <- newMVar ([])
  mapM_ (send msg newConns) conns
  a <- readMVar newConns
  putMVar shareData (msg,a)
  where send msg mvar conn  = send_ conn msg  mvar `catch` errorHandle

        errorHandle :: ConnectionException -> IO ()
        errorHandle e = pure ()

        send_ conn msg mvar = do
          sendTextData conn msg
          modifyMVar_ mvar $ \a -> pure $ conn:a


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