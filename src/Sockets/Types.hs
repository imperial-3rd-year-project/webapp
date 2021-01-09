{-# LANGUAGE GADTs #-}

module Sockets.Types where

import           Control.Concurrent           (MVar)
import           Data.ByteString              (ByteString)
import           Data.Proxy                   (Proxy)
import qualified Data.Vector.Storable         as S
import qualified Data.Text                    as T
import qualified Graphics.Capture.V4L2.Device as Device
import qualified Graphics.Display.Class       as Disp
import           Graphics.Image
import           Grenade
import qualified Network.WebSockets           as WS

data ServerState 
  = ServerState { webcam   :: Maybe (Device.Device Disp.S)
                , callback :: Maybe (S.Vector Word8 -> MVar ServerState -> WS.Connection -> IO ())
                , resnet   :: ResNet18
                , yolo     :: TinyYoloV2
                , superres :: SuperResolution
                , refBg    :: Maybe (Image VS RGBA Double)
                , newBg    :: Image VS RGBA Double
                }

newServerState :: IO ServerState
newServerState = do
  Right resnet   <- getPathForNetwork ResNet18        >>= loadResNet
  Right yolonet  <- getPathForNetwork TinyYoloV2      >>= loadTinyYoloV2
  Right supernet <- getPathForNetwork SuperResolution >>= loadSuperResolution
  
  return ServerState 
    { webcam   = Nothing
    , callback = Nothing
    , resnet   = resnet
    , yolo     = yolonet
    , superres = supernet
    , refBg    = Nothing
    , newBg    = makeImage (640,480) $ const $ PixelRGBA 0.0 1.0 0.0 0.0
    }

data MessageType where
  WebcamOn  :: String -> MessageType
  WebcamOff :: MessageType
  Compile   :: MessageType
  Image     :: ImageProcessor -> MessageType
  Capture   :: CaptureProcessor -> MessageType
  Error     :: String -> MessageType

data ImageProcessor = forall a . HandlesImage a => ImageProcessor a
data ImgProcProxy   = forall a . HandlesImage a => IPProxy (Proxy a)

class HandlesImage a where
  handleImage         :: a -> MVar ServerState -> WS.Connection -> IO ()
  matchImageProcessor :: Proxy a -> T.Text -> Maybe a
  getImageMsgFormat   :: Proxy a -> String

data CaptureProcessor = forall a . HandlesCapture a => CaptureProcessor a
data CaptProcProxy    = forall a . HandlesCapture a => CPProxy (Proxy a)

class HandlesCapture a where
  getCallback :: a -> MVar ServerState -> WS.Connection -> IO (S.Vector Word8 -> MVar ServerState -> WS.Connection -> IO ())
  matchCaptureProcessor :: Proxy a -> T.Text -> Maybe a
  getCaptureMsgFormat   :: Proxy a -> String
