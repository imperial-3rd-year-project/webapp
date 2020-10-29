{-# LANGUAGE OverloadedStrings
           , DeriveGeneric
           , GeneralisedNewtypeDeriving
           , DerivingStrategies
#-}

module Handler.MnistResponse where

import Data.Vector.Storable                   (Vector)
import Data.Vector.Storable.ByteString        (byteStringToVector)
import Grenade.Canvas.Helpers
import Import                          hiding (Vector)

newtype MnistResponse = MnistResponse {unMnistResponse :: Text}
  deriving Show
  deriving newtype (FromJSON, ToJSON)

postMnistResponseR :: Handler Value
postMnistResponseR = do
  response <- requireCheckJsonBody
  net      <- liftIO $ netLoad "/home/king/Downloads/slack/mnistModel"
  let image = byteStringToVector $ encodeUtf8 $ unMnistResponse response :: Vector Word8
  return $ toJSON $ runNet' net image

