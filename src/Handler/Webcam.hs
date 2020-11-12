{-# LANGUAGE OverloadedStrings
           , DeriveGeneric
           , GeneralisedNewtypeDeriving
           , DerivingStrategies
#-}

module Handler.Webcam where

import Data.Vector.Storable.ByteString        (byteStringToVector)
import Grenade.Demos.ImageClass
import Import                          hiding (Vector)
import Data.ByteString.Base64
import qualified Debug.Trace as DB

newtype MnistResponse = MnistResponse {unMnistResponse :: Text}
  deriving Show
  deriving newtype (FromJSON, ToJSON)

postWebcamR :: Handler ()
postWebcamR = do
  {--response <- requireCheckJsonBody
  net      <- liftIO $ loadMNIST
  let image'        = encodeUtf8 $ unMnistResponse response
      Right image'' = decode image'
      image         = byteStringToVector image''
      json          = toJSON $ runNet' net image 140
  --}
  -- liftIO $ openWebcam "/dev/video1" 
  return ()

