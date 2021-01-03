{-# LANGUAGE OverloadedStrings #-}

module Sockets.Protocol where

import qualified Network.WebSockets as WS
import           Control.Concurrent (MVar, modifyMVar_, readMVar, forkIO)
import qualified Data.Vector.Storable as S
import           Data.Word8

import           Graphics.Capture.Class
import qualified Graphics.Display.Class as Disp
import qualified Graphics.Capture.V4L2.Device as Device
import           Graphics.Display.ConversionUtils (resize)
import qualified Data.Text as T
import           Control.Monad (when, guard)
import           Data.Maybe

import           Grenade.Utils.ImageNet
import           Data.Vector.Storable.ByteString
import Grenade
import Sockets.Resnet
import Sockets.Utils
import Sockets.Types
import Sockets.Yolo
import Sockets.SuperRes
import Debug.Trace

contentsOf :: T.Text -> String -> T.Text
contentsOf msg prefix = T.drop (length prefix) msg

getImageProcessor :: T.Text -> String -> Maybe ImageProcessor
getImageProcessor msg prefix
  | "RESNET" `T.isPrefixOf` contentsOf msg prefix = Just Resnet
  | "YOLO"   `T.isPrefixOf` contentsOf msg prefix = Just Yolo
  | "SUPERRES"  `T.isPrefixOf` contentsOf msg prefix = Just SuperRes
  | otherwise = Nothing

match :: T.Text -> MessageType
match msg
  | "OPEN"  `T.isPrefixOf` msg = WebcamOn $ T.unpack (contentsOf msg "OPEN ")
  | "CLOSE" `T.isPrefixOf` msg = WebcamOff
  | "IMAGE" `T.isPrefixOf` msg
    = case getImageProcessor msg "IMAGE " of
        Just processor -> Image processor
        Nothing        -> Error "IMAGE message must be followed by { RESNET | YOLO | SUPER }"
  | "CAPTURE" `T.isPrefixOf` msg
    = case getImageProcessor msg "CAPTURE " of
        Just processor -> Capture processor
        Nothing        -> Error "CAPTURE message must be followed by { RESNET | YOLO }"
  | otherwise = Error "Message type not recognised"

dispatch :: WS.Connection -> MVar ServerState -> MessageType -> IO ()
dispatch conn state msgType = do
  (_, _, _, _, res, yolo, u) <- readMVar state
  case msgType of
    WebcamOn device -> openWebcam device state
    WebcamOff       -> (readMVar state >>= closeWebcam) >> pure ()
    Image Resnet    -> processWithResnet conn res
    Image Yolo      -> processWithYolo conn yolo
    Image SuperRes  -> processWithSuperRes conn u
    Capture Resnet  -> enableCaptureWithResnet conn state
    Capture Yolo    -> enableCaptureWithYolo conn state
    Capture SuperRes -> sendErr "Cannot apply super-resolution to webcam" conn
    Error errMsg    -> sendErr errMsg conn

handle :: T.Text -> WebsiteConn -> MVar ServerState -> IO ()
handle msg (WebsiteConn conn) state = do
  print msg
  dispatch conn state (match msg)

openWebcam :: String -> MVar ServerState -> IO ()
openWebcam deviceName state = do
  let device = Device.newV4L2CaptureDevice deviceName
  opened <- openDevice device
  stream <- startCapture opened $ sendByteString state
  modifyMVar_ state $ \(conn, _, offset, proc, net, yolo, u) -> return (conn, Just stream, offset, proc, net, yolo, u)
  pure ()

processCapture :: Maybe (Double, Double)
               -> Maybe ImageProcessor
               -> S.Vector Word8
               -> WS.Connection
               -> (ResNet18, TinyYoloV2)
               -> IO ()
processCapture offset proc v conn (res, yolo) = do
  let Just offset' = offset
      Just proc'   = proc
  case proc' of
    Resnet -> captureWithResnet offset' v conn res
    Yolo   -> captureWithYolo offset' v conn yolo
    SuperRes -> undefined

sendByteString :: MVar ServerState -> S.Vector Word8 -> IO ()
sendByteString state v = do
  (website, _, offset, proc, net, yolo, u) <- readMVar state
  guard $ isJust website
  let Just (WebsiteConn conn) = website
      bs = vectorToByteString v
  WS.sendBinaryData conn bs
  when (isJust offset && isJust proc) $ do
    processCapture offset proc v conn (net, yolo)
    modifyMVar_ state $ \(site, stream, _, _, n, y, u) -> return (site, stream, Nothing, Nothing, n, y, u)

setWebsiteConn :: WS.Connection -> ServerState -> ServerState
setWebsiteConn site (_, x, o, p, n, y, u) = (Just (WebsiteConn site), x, o, p, n, y, u)

closeWebcam :: ServerState -> IO ServerState
closeWebcam (conn, stream, _, _, n, y, u) = do
  when (isJust stream) $ let Just s = stream in stopCapture s >> pure ()
  return (conn, Nothing, Nothing, Nothing, n, y, u)

closeWebcamAndDisconnect :: ServerState -> IO ServerState
closeWebcamAndDisconnect st  = do
  closeWebcam st >>= \(_, _, _, _, n, y, u) -> return (Nothing, Nothing, Nothing, Nothing, n, y, u)


