{-# LANGUAGE OverloadedStrings #-}

module Sockets.Protocol where

import qualified Network.WebSockets as WS
import           Control.Concurrent (MVar, modifyMVar_, readMVar, forkIO)
import           Control.Exception (finally)
import qualified Data.Vector.Storable as S
import           Data.Word8

import           Graphics.Capture.Class
import qualified Graphics.Capture.V4L2.Device as Device
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Control.Monad (when, guard)
import           Data.Maybe

import           Data.Vector.Storable.ByteString
import qualified Data.ByteString as BS
import           Grenade
import           Sockets.Resnet
import           Sockets.Utils
import           Sockets.Types
import           Sockets.Yolo
import           Sockets.SuperRes
import           Sockets.GreenScreen
import qualified System.IO as IO
import           System.Directory ( getCurrentDirectory )
import           Control.Concurrent.Async
import           System.Process
import qualified Pipes as P

contentsOf :: T.Text -> String -> T.Text
contentsOf msg prefix = T.drop (length prefix) msg

getImageProcessor :: T.Text -> String -> Maybe ImageProcessor
getImageProcessor msg prefix
  | "RESNET"   `T.isPrefixOf` contentsOf msg prefix = Just Resnet
  | "YOLO"     `T.isPrefixOf` contentsOf msg prefix = Just Yolo
  | "SUPERRES" `T.isPrefixOf` contentsOf msg prefix = Just SuperRes
  | "GS"       `T.isPrefixOf` contentsOf msg prefix = Just GreenScreen
  | otherwise                                       = Nothing

match :: T.Text -> MessageType
match msg
  | "OPEN"  `T.isPrefixOf` msg = WebcamOn $ T.unpack (contentsOf msg "OPEN ")
  | "CLOSE" `T.isPrefixOf` msg = WebcamOff
  | "IMAGE" `T.isPrefixOf` msg
    = case getImageProcessor msg "IMAGE " of
        Just processor -> Image processor
        Nothing        -> Error "IMAGE message must be followed by { RESNET | YOLO | SUPER | GS }"
  | "CAPTURE" `T.isPrefixOf` msg
    = case getImageProcessor msg "CAPTURE " of
        Just processor -> Capture processor
        Nothing        -> Error "CAPTURE message must be followed by { RESNET | YOLO | GS }"
  | "COMPILE" `T.isPrefixOf` msg = Compile
  | otherwise                    = Error "Message type not recognised"

dispatch :: WS.Connection -> MVar ServerState -> MessageType -> IO ()
dispatch conn state msgType = do
  ServerState { resnet = resnet', yolo = yolo', superres = superres' } <- readMVar state
  case msgType of
    WebcamOn device     -> openWebcam device state
    WebcamOff           -> (readMVar state >>= closeWebcam) >> pure ()
    Image Resnet        -> processWithResnet conn resnet'
    Image Yolo          -> processWithYolo conn yolo'
    Image SuperRes      -> processWithSuperRes conn superres'
    Image GreenScreen   -> updateNewBackground conn state
    Capture Resnet      -> enableCaptureWithResnet conn state
    Capture Yolo        -> enableCaptureWithYolo conn state
    Capture SuperRes    -> sendErr "Cannot apply super-resolution to webcam" conn
    Capture GreenScreen -> enableGreenScreen conn state
    Compile             -> compileCode conn state
    Error errMsg        -> sendErr errMsg conn

handle :: T.Text -> WebsiteConn -> MVar ServerState -> IO ()
handle msg (WebsiteConn conn) state = do
  print msg
  dispatch conn state (match msg)

openWebcam :: String -> MVar ServerState -> IO ()
openWebcam deviceName state = do
  let device = Device.newV4L2CaptureDevice deviceName
  opened <- openDevice device
  stream <- startCapture opened $ sendByteString state
  modifyMVar_ state $ \s -> return s { webcam = Just stream, refBg = Nothing }
  pure ()

processCapture :: MVar ServerState 
               -> S.Vector Word8
               -> BS.ByteString
               -> IO ()
processCapture mstate v bs = do
  state <- readMVar mstate
  let Just offset'             = offset state
      Just proc'               = imgProc state
      Just (WebsiteConn conn') = conn state
      resnet'                  = resnet state
      yolo'                    = yolo state
      stopRepeat = modifyMVar_ mstate $ \s -> return s { offset = Nothing, imgProc = Nothing }
  case proc' of
    Resnet      -> captureWithResnet offset' v conn' resnet' >> WS.sendBinaryData conn' bs >> stopRepeat
    Yolo        -> captureWithYolo   offset' v conn' yolo'   >> WS.sendBinaryData conn' bs >> stopRepeat
    SuperRes    -> undefined
    GreenScreen -> replaceBackground mstate v bs                                           >> stopRepeat

sendByteString :: MVar ServerState -> S.Vector Word8 -> IO ()
sendByteString mstate v = do
  state <- readMVar mstate
  guard $ isJust (conn state)
  let Just (WebsiteConn conn') = conn state
      bs = vectorToByteString v
      offset'  = offset  state
      imgProc' = imgProc state
  if isJust offset' && isJust imgProc'
    then processCapture mstate v bs
    else WS.sendBinaryData conn' bs

setWebsiteConn :: WS.Connection -> ServerState -> ServerState
setWebsiteConn site s = s { conn = Just (WebsiteConn site) }

closeWebcam :: ServerState -> IO ServerState
closeWebcam s = do
  let stream = webcam s
  when (isJust stream) $ let Just stream' = stream in stopCapture stream' >>= closeDevice >> pure ()
  return s { webcam = Nothing, offset = Nothing, imgProc = Nothing }

closeWebcamAndDisconnect :: ServerState -> IO ServerState
closeWebcamAndDisconnect st  = do
  closeWebcam st >>= \s -> return s { conn = Nothing }

fromHandleProducer :: IO.Handle -> P.Producer T.Text IO ()
fromHandleProducer handle = go
  where
    go = do
      txt <- P.liftIO (TIO.hGetChunk handle)
      if T.null txt
        then return ()
        else do
          P.yield txt
          go
          go

sendToConnConsumer :: WS.Connection -> T.Text -> P.Consumer T.Text IO ()
sendToConnConsumer conn text = P.liftIO (WS.sendTextData conn text)

sendOutput :: IO.Handle -> WS.Connection -> IO ()
sendOutput handle conn = flip finally (IO.hClose handle) $ do
  P.runEffect $ do
    fromHandleProducer handle P.>-> P.for P.cat (sendToConnConsumer conn)

compileCode :: WS.Connection -> MVar ServerState -> IO ()
compileCode conn mstate = do
  code <- WS.receiveData conn :: IO T.Text
  cwd <- getCurrentDirectory
  let cwd' = cwd ++ "/grenade-tutorials"
  let tmpFile = cwd' ++ "/src/circle-user.hs"
  writeFile tmpFile (T.unpack code)
  (_, Just hout, Just herr , _) <- createProcess (shell "stack run circle-user"){ cwd = Just cwd', std_out = CreatePipe, std_err = CreatePipe}
  a1 <- async $ sendOutput hout conn
  a2 <- async $ sendOutput herr conn
  waitBoth a1 a2
  pure ()
