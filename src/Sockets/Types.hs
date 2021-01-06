module Sockets.Types where

import qualified Graphics.Display.Class as Disp
import qualified Graphics.Capture.V4L2.Device as Device
import qualified Network.WebSockets as WS
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as S
import           Data.Word8
import           Graphics.Image
import           Graphics.Image.ColorSpace
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

