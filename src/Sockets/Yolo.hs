{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}

module Sockets.Yolo where

import qualified Data.Vector.Storable as S
import           Data.Word8
import           Grenade
import qualified Data.ByteString.Lazy as BSL
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Static as H
import qualified Data.Array as A
import           Data.Function (on)
import           Data.List (sortBy)
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import           Sockets.Utils
import           Sockets.Types
import           Control.Concurrent (MVar, modifyMVar_, forkIO)
import           Grenade.Utils.PascalVoc
import           Grenade.Networks.TinyYoloV2
import           Control.Monad (forM_)

import qualified Graphics.Capture.V4L2.Device as Device
import           Graphics.Display.ConversionUtils (resize)

enableCaptureWithYolo :: WS.Connection -> MVar ServerState -> IO ()
enableCaptureWithYolo site state = do
  let offset = (112, 32) :: (Double, Double)
  modifyMVar_ state $ \(conn, stream, _, p, res, yolo, u) -> return (conn, stream, Just offset, Just Yolo, res, yolo, u)
  pure ()

captureWithYolo :: (Double, Double)
                  -> S.Vector Word8
                  -> WS.Connection
                  -> TinyYoloV2
                  -> IO ()
captureWithYolo (x, y) v conn yolo = do
  let v'          = resize (floor x, floor y) Device.v4l2resolution (416, 416) v
  _ <- forkIO $ do
    let input = preprocessYolo' v'
    boxes <- runYolo input yolo
    print boxes
    WS.sendTextData conn ("BEGIN YOLO" :: T.Text)
    forM_ boxes $ \(l, r, t, b, _, label) -> do
      WS.sendTextData conn (T.pack label)
      WS.sendTextData conn (T.pack $ show l)
      WS.sendTextData conn (T.pack $ show r)
      WS.sendTextData conn (T.pack $ show t)
      WS.sendTextData conn (T.pack $ show b)
    WS.sendTextData conn ("END YOLO" :: T.Text)
    WS.sendTextData conn ("RESUME FEED" :: T.Text)
  pure ()

processWithYolo :: WS.Connection -> TinyYoloV2 -> IO ()
processWithYolo site yolo = do
  imageData <- WS.receiveDataMessage site :: IO WS.DataMessage
  let (WS.Binary bs) = imageData
      decoded        = decodeForYolo bs 1
  input <- preprocessYolo (A.listArray (0, length decoded - 1) decoded)
  boxes <- runYolo input yolo
  WS.sendTextData site ("BEGIN YOLO" :: T.Text)
  forM_ boxes $ \(l, r, t, b, _, label) -> do
      WS.sendTextData site (T.pack label)
      WS.sendTextData site (T.pack $ show l)
      WS.sendTextData site (T.pack $ show r)
      WS.sendTextData site (T.pack $ show t)
      WS.sendTextData site (T.pack $ show b)
  WS.sendTextData site ("END YOLO" :: T.Text)

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
runYolo input yolo = do
  let y = runNet yolo input
      boxes = processOutput y 0.3
  return boxes
