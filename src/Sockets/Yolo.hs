{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}

module Sockets.Yolo where

import           Control.Concurrent               (MVar, modifyMVar_, forkIO, readMVar)
import           Control.Monad                    (forM_)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL
import qualified Data.Vector.Storable             as S
import           Data.Proxy                       (Proxy)
import qualified Data.Text                        as T
import           Data.Word8                       (Word8)
import qualified Graphics.Capture.V4L2.Device     as Device
import           Graphics.Display.ConversionUtils (resize)
import           Grenade
import           Grenade.Utils.PascalVoc          (DetectedObject, processOutput)
import qualified Network.WebSockets               as WS
import qualified Numeric.LinearAlgebra.Static     as H
import           Sockets.Types
import           Sockets.Webcam                   (closeWebcam)

data YoloProcessor = YoloProcessor

instance HandlesImage YoloProcessor where
  handleImage _ stateref socket = do
    WS.Binary imageBS <- WS.receiveDataMessage socket
    state             <- readMVar stateref
    sendYoloOutput socket $ postProcessYolo False
                          $ runNet (yolo state)
                          $ preprocessImageYolo (BSL.toStrict imageBS)
  matchImageProcessor = matchYoloProcessor
  getImageMsgFormat   = getYoloMsgFormat

instance HandlesCapture YoloProcessor where
  getCallback _ _ _     = return captureWithYolo
  matchCaptureProcessor = matchYoloProcessor
  getCaptureMsgFormat   = getYoloMsgFormat

matchYoloProcessor :: Proxy YoloProcessor -> T.Text -> Maybe YoloProcessor
matchYoloProcessor _ proc
  | proc == "YOLO" = Just YoloProcessor
  | otherwise      = Nothing

getYoloMsgFormat :: Proxy YoloProcessor -> String
getYoloMsgFormat = const "YOLO"

preprocessWebcamYolo :: S.Vector Word8 -> S ('D3 416 416 3)
preprocessWebcamYolo v = S3D (H.build gen)
  where
    gen :: Double -> Double -> Double
    gen i j = fromIntegral (v S.! x) / 255
      where
        channel = floor (i / 416)
        row     = floor i - (channel * 416)
        x       = ((row * 416) + floor j) * 3 + channel

preprocessImageYolo :: BS.ByteString -> S ('D3 416 416 3)
preprocessImageYolo bs = S3D (H.build gen)
  where
    gen :: Double -> Double -> Double
    gen i j = fromIntegral (BS.index bs x) / 255
      where
        channel = floor (i / 416)
        row     = floor i - (channel * 416)
        x       = ((row * 416) + floor j) * 4 + channel

captureWithYolo :: S.Vector Word8 -> MVar ServerState -> WS.Connection -> IO ()
captureWithYolo imageV stateref socket = do
  modifyMVar_ stateref closeWebcam
  _ <- forkIO $ do
    state <- readMVar stateref
    sendYoloOutput socket $ postProcessYolo True
                          $ runNet (yolo state)
                          $ preprocessWebcamYolo
                          $ resize (112, 32) Device.v4l2resolution (416, 416) imageV
  return ()

postProcessYolo :: Bool -> S ('D3 13 13 125) -> [DetectedObject]
postProcessYolo isFlipped output = flipOnWebcam $ processOutput output 0.3 0.5
  where
    flipOnWebcam
      | isFlipped = map (\(l, r, t, b, c, label) -> (528 - l, 448 - r, t, b, c, label))
      | otherwise = id

sendYoloOutput :: WS.Connection -> [DetectedObject] -> IO ()
sendYoloOutput socket boxes = do
  WS.sendTextData socket ("BEGIN YOLO" :: T.Text)
  forM_ boxes $ \(l, r, t, b, _, label) -> do
    WS.sendTextData socket (T.pack label)
    WS.sendTextData socket (T.pack $ show l)
    WS.sendTextData socket (T.pack $ show r)
    WS.sendTextData socket (T.pack $ show t)
    WS.sendTextData socket (T.pack $ show b)
  WS.sendTextData socket ("END YOLO" :: T.Text)
