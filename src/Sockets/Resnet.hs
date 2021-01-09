{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}


module Sockets.Resnet where

import           Control.Concurrent               (MVar, forkIO, readMVar)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL
import           Data.Function                    (on)
import           Data.List                        (maximumBy)
import           Data.Proxy                       (Proxy)
import qualified Data.Text                        as T
import qualified Data.Vector.Storable             as S
import           Data.Word8                       (Word8)
import qualified Graphics.Capture.V4L2.Device     as Device
import           Graphics.Display.ConversionUtils (resize)
import           Grenade
import           Grenade.Utils.ImageNet           (getLabel)
import qualified Network.WebSockets               as WS
import qualified Numeric.LinearAlgebra            as LA
import qualified Numeric.LinearAlgebra.Static     as H
import           Sockets.Types

data ResNetProcessor = ResNetProcessor

instance HandlesImage ResNetProcessor where
  handleImage _ stateref socket = do
    WS.Binary imageBS <- WS.receiveDataMessage socket
    state             <- readMVar stateref
    sendResNetOutput socket $ postProcessResNet
                            $ runNet (resnet state)
                            $ preprocessImageResNet (BSL.toStrict imageBS)
  matchImageProcessor = matchResNetProcessor
  getImageMsgFormat   = getResNetMsgFormat

instance HandlesCapture ResNetProcessor where
  getCallback _ _ socket = do
    offset <- WS.receiveData socket
    return (captureWithResNet $ read $ T.unpack offset)
  matchCaptureProcessor = matchResNetProcessor
  getCaptureMsgFormat   = getResNetMsgFormat

matchResNetProcessor :: Proxy ResNetProcessor -> T.Text -> Maybe ResNetProcessor
matchResNetProcessor _ proc
  | proc == "RESNET" = Just ResNetProcessor
  | otherwise      = Nothing

getResNetMsgFormat :: Proxy ResNetProcessor -> String
getResNetMsgFormat = const "RESNET"

preprocessWebcamResNet :: S.Vector Word8 -> S ('D3 224 224 3)
preprocessWebcamResNet v = S3D (H.build gen)
  where
    gen :: Double -> Double -> Double
    gen i j
      | channel == 0 = (e - 0.485) / 0.229
      | channel == 1 = (e - 0.456) / 0.224
      | channel == 2 = (e - 0.406) / 0.225
      | otherwise    = error "preprocessWebcamResNet: Channel not between 0 and 2."
      where
        channel = floor (i / 224)
        row     = floor i - (channel * 224)
        x       = ((row * 224) + floor j) * 3 + channel
        e       = fromIntegral (v S.! x) / 255

preprocessImageResNet :: BS.ByteString -> S ('D3 224 224 3)
preprocessImageResNet bs = S3D (H.build gen)
  where
    gen :: Double -> Double -> Double
    gen i j
      | channel == 0 = (e - 0.485) / 0.229
      | channel == 1 = (e - 0.456) / 0.224
      | channel == 2 = (e - 0.406) / 0.225
      | otherwise    = error "preprocessImageResNet: Channel not between 0 and 2."
      where
        channel = floor (i / 224)
        row     = floor i - (channel * 224)
        x       = ((row * 224) + floor j) * 4 + channel
        e       = fromIntegral (BS.index bs x) / 255

captureWithResNet :: (Double, Double) -> S.Vector Word8 -> MVar ServerState -> WS.Connection -> IO ()
captureWithResNet (x, y) imageV stateref socket = do
  _ <- forkIO $ do
    state <- readMVar stateref
    sendResNetOutput socket $ postProcessResNet
                            $ runNet (resnet state)
                            $ preprocessWebcamResNet
                            $ resize (416 - floor x, floor y) Device.v4l2resolution (224, 224) imageV
  return ()

postProcessResNet :: S ('D1 1000) -> String
postProcessResNet (S1D v) = getLabel $ fst $ maximumBy (compare `on` snd) $ zip [0..] $ LA.toList $ H.extract v

sendResNetOutput :: WS.Connection -> String -> IO ()
sendResNetOutput socket label = WS.sendTextData socket ("RESNET " <> T.pack label)
