{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE OverloadedStrings #-}


module Sockets.Resnet where

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
import           Control.Monad (forM_)
import           Grenade.Utils.ImageNet

import qualified Graphics.Capture.V4L2.Device as Device
import           Graphics.Display.ConversionUtils (resize)

enableCaptureWithResnet :: WS.Connection -> MVar ServerState -> IO ()
enableCaptureWithResnet site state = do
  xMsg <- WS.receiveData site :: IO T.Text
  yMsg <- WS.receiveData site :: IO T.Text
  let offset' = (read $ T.unpack xMsg, read $ T.unpack yMsg) :: (Double, Double)
  modifyMVar_ state $ \s -> return s { offset = Just offset', imgProc = Just Resnet }
  pure ()

captureWithResnet :: (Double, Double)
                  -> S.Vector Word8
                  -> WS.Connection
                  -> ResNet18
                  -> IO ()
captureWithResnet (x, y) v conn res = do
  let v'          = resize (floor x, floor y) Device.v4l2resolution (224, 224) v
  _ <- forkIO $ do
    let input = preprocessResnet' v'
    out <- runResNet res input
    case out of
      Left err    -> WS.sendTextData conn (T.pack ("ERR " ++ err))
      Right probs -> do
        WS.sendTextData conn ("BEGIN RESNET" :: T.Text)
        forM_ [head probs] $ \(c, _) -> do
          WS.sendTextData conn (T.pack (getLabel c))
        WS.sendTextData conn ("END RESNET" :: T.Text)
        WS.sendTextData conn ("RESUME FEED" :: T.Text)
  pure ()

processWithResnet :: WS.Connection -> ResNet18 -> IO ()
processWithResnet site res = do
  imageData <- WS.receiveDataMessage site :: IO WS.DataMessage
  let (WS.Binary bs) = imageData
      decoded        = decodeForResnet bs 1
  input <- preprocessResnet (A.listArray (0, length decoded - 1) decoded)
  out <- runResNet res input
  case out of
    Left err    -> sendErr err site
    Right probs -> do
      WS.sendTextData site ("BEGIN RESNET" :: T.Text)
      forM_ [head probs] $ \(c, _) -> do
        WS.sendTextData site (T.pack (getLabel c))
      WS.sendTextData site ("END RESNET" :: T.Text)

decodeForResnet :: BSL.ByteString -> Int -> [Double]
decodeForResnet bs i
  | BSL.null bs = []
  | i `mod` 4 == 0  = decodeForResnet (BSL.tail bs) 1
  | otherwise   = f : decodeForResnet (BSL.tail bs) (i + 1)
  where
    e = fromIntegral $ BSL.head bs :: Int
    e' = fromIntegral e / 255 :: Double
    f | i == 1 = (e' - 0.485) / 0.229
      | i == 2 = (e' - 0.456) / 0.224
      | i == 3 = (e' - 0.406) / 0.225

preprocessResnet :: A.Array Int Double -> IO (S ('D3 224 224 3))
preprocessResnet pixels = do
  let mat = H.build buildMat
  pure $ S3D mat
  where
    buildMat :: Double -> Double -> Double
    buildMat i j = pixels A.! x
      where
        chn = floor $ i / 224 :: Int
        row = i - fromIntegral (chn * 224)
        x   = floor $ ((row * 224) + j) * 3 + fromIntegral chn

preprocessResnet' :: S.Vector Word8 -> S ('D3 224 224 3)
preprocessResnet' v = S3D mat
  where
    mat = H.build buildMat

    buildMat :: Double -> Double -> Double
    buildMat i j = f
      where
        chn = floor $ i / 224 :: Int
        row = i - fromIntegral (chn * 224)
        x   = floor $ ((row * 224) + j) * 3 + fromIntegral chn
        e   = fromIntegral (v S.! x) / 255

        f | chn == 0 = (e - 0.485) / 0.229
          | chn == 1 = (e - 0.456) / 0.224
          | chn == 2 = (e - 0.406) / 0.225

runResNet :: ResNet18 -> S ('D3 224 224 3) -> IO (Either String [(Int, Double)])
runResNet res input = do
  -- Todo: load the network once, at the start of the application
  let S1D y = runNet res input
      tops  = getTop 5 $ LA.toList $ H.extract y
  return $ Right tops
  where
    getTop :: Ord a => Int -> [a] -> [(Int, a)]
    getTop n xs = take n $ sortBy (flip compare `on` snd) $ zip [0..] xs
