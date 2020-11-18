{-# LANGUAGE OverloadedStrings #-}

module Sockets.Webcam where

import Graphics.Capture.Class
import qualified Graphics.Display.Class as Disp
import qualified Graphics.Capture.V4L2.Device as Device
import qualified Network.WebSockets as WS
import Control.Concurrent (MVar, modifyMVar_, readMVar)
import Control.Exception (finally)
import qualified Data.Text as T
import Data.Maybe
import Control.Monad (forever, guard, when)
import Data.Vector.Storable
import Data.Word8
import Data.ByteString.Internal as BSI

sendByteString :: ServerState -> Vector Word8 -> IO ()
sendByteString (website, _) v = do
  guard $ isJust website
  let Just (WebsiteConn conn) = website
  let (ptr, off, len) = unsafeToForeignPtr v
  let bs = BSI.fromForeignPtr ptr off len
  WS.sendBinaryData conn bs
  pure ()

openWebcam :: String -> MVar ServerState -> IO ()
openWebcam deviceName state = do
  let device = Device.Unopened deviceName
  opened <- openDevice device
  server <- readMVar state
  stream <- startCapture opened $ sendByteString server 
  modifyMVar_ state $ \(conn, _) -> return (conn, Just stream)
  pure ()

startServer :: MVar ServerState -> IO ()
startServer state = do
  putStrLn "Server started!"
  WS.runServer "127.0.0.1" 9160 $ application state

newtype WebsiteConn = WebsiteConn WS.Connection

type ServerState = (Maybe WebsiteConn, Maybe (Device.Device Disp.S))

newServerState :: ServerState
newServerState = (Nothing, Nothing)

setWebsiteConn :: WS.Connection -> ServerState -> ServerState
setWebsiteConn site (_, x) = (Just (WebsiteConn site), x)

closeWebcam :: ServerState -> IO ServerState
closeWebcam (conn, stream) = do
  when (isJust stream) $ let Just s = stream in stopCapture s >> pure ()
  return (conn, Nothing)

closeWebcamAndDisconnect :: ServerState -> IO ServerState
closeWebcamAndDisconnect st  = do
  closeWebcam st >> return (Nothing, Nothing)

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 5 (return ()) $ do
    msg <- (WS.receiveData conn) :: IO T.Text
    let isWebsite = "website" `T.isPrefixOf` msg
    case msg of
      _ | not isWebsite -> WS.sendTextData conn ("First message must declare client" :: T.Text)
        | otherwise -> flip finally (disconnect state) $ do
            modifyMVar_ state $ \s -> do
              let s' = setWebsiteConn conn s
              WS.sendTextData conn ("Welcome!" :: T.Text)
              return s'
            talkWebsite (WebsiteConn conn) state

disconnect :: MVar ServerState -> IO ()
disconnect state = do
  modifyMVar_ state $ \s -> do
    s' <- closeWebcamAndDisconnect s
    return s'
  pure ()

-- Relay website messages to the webcam
-- e.g. when to open or close the webcam stream
talkWebsite :: WebsiteConn -> MVar ServerState -> IO ()
talkWebsite (WebsiteConn site) state = forever $ do
  msg <- (WS.receiveData site) :: IO T.Text
  putStrLn "Message recevied!"
  putStrLn (T.unpack msg)
  if ("open" `T.isPrefixOf` msg)
     then openWebcam "/dev/video1" state -- Later on, we will use the msg to determine the device
     else (readMVar state >>= closeWebcam) >> pure ()
  pure ()

