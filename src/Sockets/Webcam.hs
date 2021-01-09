{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}

module Sockets.Webcam where


import           Control.Concurrent              (MVar, modifyMVar_, readMVar)
import           Control.Monad                   ((>=>))
import qualified Data.Vector.Storable            as S
import           Data.Vector.Storable.ByteString
import           Data.Word8                      (Word8)
import           Graphics.Capture.Class
import qualified Graphics.Capture.V4L2.Device as Device
import           Graphics.Utils.Types            (U)
import qualified Network.WebSockets              as WS
import           Sockets.Types
import           System.FilePath.Posix           (takeFileName)

type WebcamDevice = (FilePath, String)

getWebcamDevices :: IO [WebcamDevice]
getWebcamDevices = getDevices >>= mapM getName
  where
    getName :: Device.Device U -> IO WebcamDevice
    getName dev = do
      let path = deviceDescription dev
      name <- readFile $ "/sys/class/video4linux/" ++ takeFileName path ++ "/name"
      pure (path, name)

openWebcam :: String -> MVar ServerState -> WS.Connection -> IO ()
openWebcam deviceName state socket = do
  opened <- openDevice (Device.newV4L2CaptureDevice deviceName)
  stream <- startCapture opened $ sendByteString state socket
  modifyMVar_ state $ \s -> return s { webcam = Just stream, refBg = Nothing }

closeWebcam :: ServerState -> IO ServerState
closeWebcam state = do
  sequence_ $ (stopCapture >=> closeDevice) <$> webcam state
  return state { webcam = Nothing, callback = Nothing }

-- | Retrieves callback from server state describing what to do with image taken by webcam.
--   Clears the callback after, so we don't have repeats. If there is no callback simply
--   send the image through the socket
sendByteString :: MVar ServerState -> WS.Connection -> S.Vector Word8 -> IO ()
sendByteString stateref socket v = do
  state <- readMVar stateref
  maybe (WS.sendBinaryData socket $ vectorToByteString v)
        (\callback' -> do
          callback' v stateref socket
          modifyMVar_ stateref $ \s -> return s { callback = Nothing })
        (callback state)
