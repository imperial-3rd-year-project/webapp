{-# LANGUAGE OverloadedStrings
           , DeriveGeneric
           , GeneralisedNewtypeDeriving
           , DerivingStrategies
#-}

module Handler.Webcam where

import Data.Vector.Storable.ByteString        (byteStringToVector)
import Import                          hiding (Vector)
import Data.ByteString.Base64
import qualified Debug.Trace as DB

newtype MnistResponse = MnistResponse {unMnistResponse :: Text}
  deriving Show
  deriving newtype (FromJSON, ToJSON)

postWebcamR :: Handler ()
postWebcamR = do
  return ()

