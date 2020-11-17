{-# LANGUAGE OverloadedStrings
           , DeriveGeneric
           , GeneralisedNewtypeDeriving
           , DerivingStrategies
#-}

module Handler.MnistResponse where

import Data.Vector.Storable.ByteString        (byteStringToVector)
import Grenade.Assets.Paths
import Grenade.Demos.MNIST
import Grenade.Assets.Paths
import Import                          hiding (Vector)
import Data.ByteString.Base64
import qualified Debug.Trace as DB

newtype MnistResponse = MnistResponse {unMnistResponse :: Text}
  deriving Show
  deriving newtype (FromJSON, ToJSON)

postMnistResponseR :: Handler Value
postMnistResponseR = do
  response  <- requireCheckJsonBody
  mnistPath <- liftIO $ getPathForNetwork MNIST
  net <- liftIO $ (loadNetwork mnistPath :: IO MNIST)
  let image'        = encodeUtf8 $ unMnistResponse response
      Right image'' = decode image'
      image         = byteStringToVector image''
  return $ toJSON $ runNet' net image 140

