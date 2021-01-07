module Sockets.Types where

import qualified Graphics.Display.Class as Disp
import qualified Graphics.Capture.V4L2.Device as Device
import qualified Network.WebSockets as WS
import           Graphics.Image
import Grenade

newtype WebsiteConn = WebsiteConn WS.Connection

data ServerState 
  = ServerState { conn     :: Maybe WebsiteConn
                , webcam   :: Maybe (Device.Device Disp.S)
                , offset   :: Maybe (Double, Double)
                , imgProc  :: Maybe ImageProcessor
                , resnet   :: ResNet18
                , yolo     :: TinyYoloV2
                , superres :: SuperResolution
                , refBg    :: Maybe (Image VS RGBA Double)
                , newBg    :: Image VS RGBA Double
                }

newServerState :: IO ServerState
newServerState = do
  Right resnet   <- getPathForNetwork ResNet18 >>= loadResNet
  Right yolonet  <- getPathForNetwork TinyYoloV2 >>= loadTinyYoloV2
  Right supernet <- getPathForNetwork SuperResolution >>= loadSuperResolution
  
  return ServerState 
    { conn     = Nothing
    , webcam   = Nothing
    , offset   = Nothing
    , imgProc  = Nothing
    , resnet   = resnet
    , yolo     = yolonet
    , superres = supernet
    , refBg    = Nothing
    , newBg    = makeImage (640,480) $ const $ PixelRGBA 0.0 1.0 0.0 0.0
    }

data ImageProcessor = Resnet | Yolo | SuperRes | GreenScreen

data MessageType
  = WebcamOn String
  | WebcamOff
  | Image   ImageProcessor
  | Capture ImageProcessor
  | Compile
  | Error   String

