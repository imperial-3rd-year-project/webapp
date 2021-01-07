module Sockets.Utils where

import qualified Network.WebSockets as WS
import qualified Data.Text as T
import           Sockets.Types
import           Graphics.Capture.Class
import qualified Graphics.Capture.V4L2.Device as Device
import           Data.Maybe
import           Control.Monad (when)

sendErr :: String -> WS.Connection -> IO ()
sendErr errMsg conn = WS.sendTextData conn (T.pack ("ERR " ++ errMsg))

closeWebcam :: ServerState -> IO ServerState
closeWebcam s = do
  let stream = webcam s
  when (isJust stream) $ let Just stream' = stream in stopCapture stream' >>= closeDevice >> pure ()
  return s { webcam = Nothing, offset = Nothing, imgProc = Nothing }

closeWebcamAndDisconnect :: ServerState -> IO ServerState
closeWebcamAndDisconnect st  = do
  closeWebcam st >>= \s -> return s { conn = Nothing }