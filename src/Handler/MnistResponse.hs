{-# LANGUAGE OverloadedStrings
           , DeriveGeneric
           , GeneralisedNewtypeDeriving
           , DerivingStrategies
#-}

module Handler.MnistResponse where

import qualified Data.Vector.Storable  as V
import Data.Vector.Storable.ByteString        (byteStringToVector)
import Grenade.Canvas.Helpers
import Import                          hiding (Vector)
import Data.ByteString.Base64
import Data.Either (fromRight)

newtype MnistResponse = MnistResponse {unMnistResponse :: Text}
  deriving Show
  deriving newtype (FromJSON, ToJSON)

postMnistResponseR :: Handler Value
postMnistResponseR = do
  response <- requireCheckJsonBody
  net      <- liftIO $ netLoad "/Users/gabymarfani/Desktop/imperial/year_3/group_project/grenade/models/mnistModel"
  let image'        = encodeUtf8 $ unMnistResponse response
      Right image'' = decode image'
      image         = byteStringToVector image''
  traceM $  show $ V.length image
  --liftIO $ Prelude.putStrLn "aaaaaa help me please"
  --liftIO $ Prelude.putStrLn $ show $ decode image'
  return $ toJSON $ runNet' net image

