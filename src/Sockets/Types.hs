module Sockets.Types where

import qualified Graphics.Display.Class as Disp
import qualified Graphics.Capture.V4L2.Device as Device
import qualified Network.WebSockets as WS
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
                }

newServerState :: IO ServerState
newServerState = do
  Right res   <- getPathForNetwork ResNet18 >>= loadResNet
  Right yolo  <- getPathForNetwork TinyYoloV2 >>= loadTinyYoloV2
  Right super <- getPathForNetwork SuperResolution >>= loadSuperResolution
  
  return ServerState 
    { conn     = Nothing
    , webcam   = Nothing
    , offset   = Nothing
    , imgProc  = Nothing
    , resnet   = res
    , yolo     = yolo
    , superres = super
    }


data ImageProcessor = Resnet | Yolo | SuperRes

data MessageType
  = WebcamOn String
  | WebcamOff
  | Image   ImageProcessor
  | Capture ImageProcessor
  | Compile
  | Error   String

