{-# LANGUAGE OverloadedStrings
           , DeriveGeneric
           , GeneralisedNewtypeDeriving
           , DerivingStrategies
#-}

module Handler.MnistResponse where

import Data.Vector.Storable.ByteString        (byteStringToVector)
import Grenade.Assets.Paths
import Grenade.Demos.MNIST
import Import                          hiding (Vector)
import Data.ByteString.Base64

newtype MnistResponse = MnistResponse {unMnistResponse :: Text}
  deriving Show
  deriving newtype (FromJSON, ToJSON)

postMnistResponseR :: Handler Value
postMnistResponseR = do
  return "test the circle"
  response <- requireCheckJsonBody
  mnistPath <- liftIO $ getPathForNetwork MNIST
  net       <- liftIO $ loadNetwork mnistPath
  let image'        = encodeUtf8 $ unMnistResponse response
      Right image'' = decode image'
      image         = byteStringToVector image''
  traceM $  show $ V.length image
  return $ toJSON $ runNet' net image 224

