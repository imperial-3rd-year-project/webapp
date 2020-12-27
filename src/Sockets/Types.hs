module Sockets.Types where

import qualified Graphics.Display.Class as Disp
import qualified Graphics.Capture.V4L2.Device as Device
import qualified Network.WebSockets as WS
import Grenade

newtype WebsiteConn = WebsiteConn WS.Connection

type ServerState = (Maybe WebsiteConn, Maybe (Device.Device Disp.S), Maybe (Double, Double), Maybe ImageProcessor, ResNet18, TinyYoloV2, SuperResolution)

newServerState :: IO ServerState
newServerState = do
  Right res <- getPathForNetwork ResNet18 >>= loadResNet

  Right yolo <- getPathForNetwork TinyYoloV2 >>= loadTinyYoloV2
  
  Right super <- getPathForNetwork SuperResolution >>= loadSuperResolution
  
  return (Nothing, Nothing, Nothing, Nothing, res, yolo, super)

data ImageProcessor = Resnet | Yolo | SuperRes

data MessageType
  = WebcamOn String
  | WebcamOff
  | Image   ImageProcessor
  | Capture ImageProcessor
  | Error   String

