{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}

module Sockets.Yolo where

import qualified Data.Vector.Storable as S
import           Data.Word8
import           Grenade
import qualified Data.ByteString.Lazy as BSL
import qualified Numeric.LinearAlgebra.Static as H
import qualified Data.Array as A
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import           Sockets.Utils

import           Sockets.Types
import           Control.Concurrent (MVar, modifyMVar_, forkIO)
import           Grenade.Utils.PascalVoc
import           Control.Monad (forM_)

import qualified Graphics.Capture.V4L2.Device as Device
import           Graphics.Display.ConversionUtils (resize)

enableCaptureWithYolo :: WS.Connection -> MVar ServerState -> IO ()
enableCaptureWithYolo _ state = do
  let offset' = (112, 32) :: (Double, Double)
  modifyMVar_ state $ \s -> return s {offset = Just offset', imgProc = Just Yolo}
  pure ()

captureWithYolo :: (Double, Double)
                  -> S.Vector Word8
                  -> WS.Connection
                  -> TinyYoloV2
                  -> MVar ServerState
                  -> IO ()
captureWithYolo (x, y) v conn net mstate = do
  let v' = resize (floor x, floor y) Device.v4l2resolution (416, 416) v
  modifyMVar_ mstate closeWebcam
  _ <- forkIO $ do
    let input = preprocessYolo' v'
    boxes <- runYolo input net
    print boxes
    let offsetW = (640 - 416) `div` 2
        offsetH = (480 - 416) `div` 2
    WS.sendTextData conn ("BEGIN YOLO" :: T.Text)
    forM_ boxes $ \(l, r, t, b, _, label) -> do
      WS.sendTextData conn (T.pack label)
      WS.sendTextData conn (T.pack $ show $ (416 - l) + offsetW) -- We flip the image on the x axis when drawing.
      WS.sendTextData conn (T.pack $ show $ (416 - r) + offsetW)
      WS.sendTextData conn (T.pack $ show $ t + offsetH)
      WS.sendTextData conn (T.pack $ show $ b + offsetH)
    WS.sendTextData conn ("END YOLO" :: T.Text)
  pure ()

processWithYolo :: WS.Connection -> TinyYoloV2 -> IO ()
processWithYolo conn net = do
  imageData <- WS.receiveDataMessage conn :: IO WS.DataMessage
  let (WS.Binary bs) = imageData
      decoded        = decodeForYolo bs 1
  input <- preprocessYolo (A.listArray (0, length decoded - 1) decoded)
  boxes <- runYolo input net
  WS.sendTextData conn ("BEGIN YOLO" :: T.Text)
  forM_ boxes $ \(l, r, t, b, _, label) -> do
      WS.sendTextData conn (T.pack label)
      WS.sendTextData conn (T.pack $ show l)
      WS.sendTextData conn (T.pack $ show r)
      WS.sendTextData conn (T.pack $ show t)
      WS.sendTextData conn (T.pack $ show b)
  WS.sendTextData conn ("END YOLO" :: T.Text)

decodeForYolo :: BSL.ByteString -> Int -> [Double]
decodeForYolo bs i
  | BSL.null bs = []
  | i `mod` 4 == 0  = decodeForYolo (BSL.tail bs) 1
  | otherwise       = e' : decodeForYolo (BSL.tail bs) (i + 1)
  where
    e = fromIntegral $ BSL.head bs :: Int
    e' = fromIntegral e / 255 :: Double

preprocessYolo :: A.Array Int Double -> IO (S ('D3 416 416 3))
preprocessYolo pixels = do
  let mat = H.build buildMat
  pure $ S3D mat
  where
    buildMat :: Double -> Double -> Double
    buildMat i j = pixels A.! x
      where
        chn = floor $ i / 416 :: Int
        row = i - fromIntegral (chn * 416)
        x   = floor $ ((row * 416) + j) * 3 + fromIntegral chn

preprocessYolo' :: S.Vector Word8 -> S ('D3 416 416 3)
preprocessYolo' v = S3D mat
  where
    mat = H.build buildMat

    buildMat :: Double -> Double -> Double
    buildMat i j = e
      where
        chn = floor $ i / 416 :: Int
        row = i - fromIntegral (chn * 416)
        x   = floor $ ((row * 416) + j) * 3 + fromIntegral chn
        e   = fromIntegral (v S.! x) / 255

runYolo :: S ('D3 416 416 3) -> TinyYoloV2 -> IO [DetectedObject]
runYolo input net = do
  let y = runNet net input
      boxes = processOutput y 0.3 0.5
  return boxes
