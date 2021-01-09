{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE Rank2Types        #-}

module Sockets.Protocol where

import           Control.Applicative             ((<|>), empty)
import           Control.Concurrent              (MVar, modifyMVar_, readMVar)
import           Control.Concurrent.Async        (async, waitBoth)
import           Control.Exception               (finally)
import           Data.List                       (intercalate, foldl')
import           Data.Proxy                      (Proxy(..))
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO
import qualified Network.WebSockets              as WS
import qualified Pipes                           as P
import           Sockets.GreenScreen
import           Sockets.Resnet
import           Sockets.SuperRes
import           Sockets.Types
import           Sockets.Utils
import           Sockets.Webcam
import           Sockets.Yolo
import           System.Directory                (getCurrentDirectory)
import qualified System.IO                       as IO
import           System.Process                  hiding (proc)

imageProcessors :: [ImgProcProxy]
imageProcessors = [ IPProxy $ Proxy @ResNetProcessor
                  , IPProxy $ Proxy @YoloProcessor
                  , IPProxy $ Proxy @SuperResProcessor
                  , IPProxy $ Proxy @GreenScreenProcessor
                  ]

captureProcessors :: [CaptProcProxy]
captureProcessors = [ CPProxy $ Proxy @ResNetProcessor
                    , CPProxy $ Proxy @YoloProcessor
                    , CPProxy $ Proxy @GreenScreenProcessor
                    ]

handle :: T.Text ->  MVar ServerState -> WS.Connection -> IO ()
handle msg stateref socket = print msg >> dispatch (parseMessage msg) stateref socket

-- | Given a message, calls the appropriate handler to serve the request.
dispatch :: MessageType -> MVar ServerState -> WS.Connection -> IO ()
dispatch msg stateref socket = case msg of
  WebcamOn device                 -> openWebcam device stateref socket
  WebcamOff                       -> readMVar stateref >>= closeWebcam >> pure ()
  Image (ImageProcessor proc)     -> handleImage proc stateref socket
  Capture (CaptureProcessor proc) -> do
    newcallback <- getCallback proc stateref socket
    modifyMVar_ stateref (\s -> return s { callback = Just newcallback })
  Compile                         -> compileCode socket
  Error errmsg                    -> sendErr errmsg socket

parseMessage :: T.Text -> MessageType
parseMessage msg
  | "OPEN"    `T.isPrefixOf` msg = WebcamOn $ T.unpack $ contentsOf msg "OPEN "
  | "CLOSE"   `T.isPrefixOf` msg = WebcamOff
  | "IMAGE"   `T.isPrefixOf` msg = either Error Image   $ getImageProcessor   $ contentsOf msg "IMAGE "
  | "CAPTURE" `T.isPrefixOf` msg = either Error Capture $ getCaptureProcessor $ contentsOf msg "CAPTURE "
  | "COMPILE" `T.isPrefixOf` msg = Compile
  | otherwise                    = Error "Message type not recognised"

getImageProcessor :: T.Text -> Either String ImageProcessor
getImageProcessor proc = maybeToEither errMsg
                       $ foldl' (<|>) empty
                       $ map (\(IPProxy px) -> ImageProcessor <$> matchImageProcessor px proc) imageProcessors
  where
    errMsg = "IMAGE message must be followed by { " ++ intercalate " | " valid ++ " }"
    valid  = map (\(IPProxy px) -> getImageMsgFormat px) imageProcessors

getCaptureProcessor :: T.Text -> Either String CaptureProcessor
getCaptureProcessor proc = maybeToEither errMsg
                         $ foldl' (<|>) empty
                         $ map (\(CPProxy px) -> CaptureProcessor <$> matchCaptureProcessor px proc) captureProcessors
  where
    errMsg = "CAPTURE message must be followed by { " ++ intercalate " | " valid ++ " }"
    valid  = map (\(CPProxy px) -> getCaptureMsgFormat px) captureProcessors

contentsOf :: T.Text -> String -> T.Text
contentsOf msg prefix = T.drop (length prefix) msg

compileCode :: WS.Connection -> IO ()
compileCode socket = do
  code <- WS.receiveData socket :: IO T.Text
  cwd'' <- getCurrentDirectory
  let cwd'    = cwd'' ++ "/grenade-tutorials"
      tmpFile = cwd'  ++ "/src/circle-user.hs"
  writeFile tmpFile (T.unpack code)
  (_, Just hout, Just herr , _) <- createProcess (shell "stack run circle-user") { cwd     = Just cwd'
                                                                                 , std_out = CreatePipe
                                                                                 , std_err = CreatePipe
                                                                                 }
  a1 <- async $ sendOutput hout socket
  a2 <- async $ sendOutput herr socket
  _  <- waitBoth a1 a2
  pure ()


fromHandleProducer :: IO.Handle -> P.Producer T.Text IO ()
fromHandleProducer h = go
  where
    go = do
      txt <- P.liftIO (TIO.hGetChunk h)
      if T.null txt
        then return ()
        else do
          P.yield txt
          go
          go

sendToConnConsumer :: WS.Connection -> T.Text -> P.Consumer T.Text IO ()
sendToConnConsumer conn text = P.liftIO (WS.sendTextData conn text)

sendOutput :: IO.Handle -> WS.Connection -> IO ()
sendOutput h conn = flip finally (IO.hClose h) $ do
  P.runEffect $ do
    fromHandleProducer h P.>-> P.for P.cat (sendToConnConsumer conn)
