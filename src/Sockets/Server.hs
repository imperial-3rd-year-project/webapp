{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}

module Sockets.Server where

import           Control.Concurrent              (MVar, modifyMVar_)
import           Control.Monad                   (forever, when)
import           Control.Exception               (finally)
import qualified Data.Text                       as T
import qualified Network.WebSockets              as WS
import           Sockets.Protocol
import           Sockets.Types
import           Sockets.Webcam

startServer :: MVar ServerState -> IO ()
startServer state = do
  putStrLn "Server started!"
  WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application stateref pending = do
  socket <- WS.acceptRequest pending
  WS.withPingThread socket 5 (return ()) $ do
    msg <- WS.receiveData socket
    flip finally (disconnect stateref) $ do
      when ("WEBCAM" `T.isPrefixOf` msg) $ do
        WS.sendTextData socket ("BEGIN DEVLIST" :: T.Text)
        getWebcamDevices >>= mapM_ (sendList socket)
        WS.sendTextData socket ("END DEVLIST" :: T.Text)
      talkWebsite stateref socket
  where
    sendList :: WS.Connection -> WebcamDevice -> IO ()
    sendList conn (path, name) = do
      WS.sendTextData conn (T.pack $ path ++ "$" ++ name)

disconnect :: MVar ServerState -> IO ()
disconnect stateref = modifyMVar_ stateref closeWebcam

--getMsgContents :: String -> T.Text -> T.Text
--getMsgContents t = T.drop (length $ t ++ " ")

-- | Relay website messages to the webcam
-- e.g. when to open or close the webcam stream
talkWebsite :: MVar ServerState -> WS.Connection -> IO ()
talkWebsite state socket = forever (WS.receiveData socket >>= (\msg -> handle msg state socket))
