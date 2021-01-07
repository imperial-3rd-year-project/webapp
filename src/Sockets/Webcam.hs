{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}

module Sockets.Webcam where

import           Graphics.Capture.Class
import qualified Graphics.Display.Class as Disp
import qualified Graphics.Capture.V4L2.Device as Device
import           Graphics.Display.ConversionUtils (resize)

import           Graphics.Utils.Types (U)
import qualified Network.WebSockets as WS
import           Control.Concurrent (MVar, modifyMVar_, readMVar, forkOS, yield)
import           Control.Exception (finally)
import qualified Data.Text as T
import           Data.Maybe
import           Control.Monad (forever, guard, when, forM_, void)
import qualified Data.Vector.Storable as S
import           Data.Word8
import           System.FilePath.Posix (takeFileName)
import           Grenade
import           Grenade.Utils.ImageNet
import           Grenade.Networks.ResNet18
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Vector.Storable.ByteString
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Data as NLA
import qualified Numeric.LinearAlgebra.Static as H
import qualified Data.Array as A
import           Grenade.Core.Shape
import           Data.Ord
import           Data.Function (on)
import           Data.List (sortBy)
import Debug.Trace (trace)

import Sockets.Protocol
import Sockets.Resnet
import Sockets.Utils
import Sockets.Types

type WebcamDevice = (FilePath, String)

getWebcamDevices :: IO [WebcamDevice]
getWebcamDevices = do
  getDevices :: IO [Device.Device U]
  >>= mapM getName
  >>= pure
    where
      getName :: Device.Device U -> IO WebcamDevice
      getName dev = do
        let path = deviceDescription dev
        name <- readFile $ "/sys/class/video4linux/" ++ takeFileName path ++ "/name"
        pure (path, name)

startServer :: MVar ServerState -> IO ()
startServer state = do
  putStrLn "Server started!"
  WS.runServer "127.0.0.1" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 5 (return ()) $ do
    msg <- WS.receiveData conn :: IO T.Text
    flip finally (disconnect state) $ do
      modifyMVar_ state $ \s -> do
        let s' = setWebsiteConn conn s
        when ("WEBCAM" `T.isPrefixOf` msg) $ do
          WS.sendTextData conn ("BEGIN DEVLIST" :: T.Text)
          getWebcamDevices >>= mapM_ (sendList conn)
          WS.sendTextData conn ("END DEVLIST" :: T.Text)
        return s'
      talkWebsite (WebsiteConn conn) state
  where
    sendList :: WS.Connection -> WebcamDevice -> IO ()
    sendList conn (path, name) = do
      WS.sendTextData conn (T.pack $ path ++ "$" ++ name)

disconnect :: MVar ServerState -> IO ()
disconnect state = do
  modifyMVar_ state $ \s -> do
    closeWebcamAndDisconnect s
  pure ()

getMsgContents :: String -> T.Text -> T.Text
getMsgContents t = T.drop (length $ t ++ " ")

-- Relay website messages to the webcam
-- e.g. when to open or close the webcam stream
talkWebsite :: WebsiteConn -> MVar ServerState -> IO ()
talkWebsite conn@(WebsiteConn site) state = forever $ do
  msg <- WS.receiveData site :: IO T.Text
  handle msg conn state
