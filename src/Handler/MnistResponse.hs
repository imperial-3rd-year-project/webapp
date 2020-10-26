{-# LANGUAGE OverloadedStrings #-}
module Handler.MnistResponse where

import Import
import Data.Aeson
import Yesod.Core.Json
import Data.ByteString

-- newtype MnistResponse = MnistResponse ByteString

-- instance ToJSON MnistResponse where
--     toJSON (MnistResponse t) = object ["canvasData" .= t]
-- instance FromJSON MnistResponse where
--     parseJSON = withObject "MnistResponse" $ \o -> MnistResponse <$> o .: "canvasData"

postMnistResponseR :: Handler Value
postMnistResponseR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    response <- requireCheckJsonBody :: Handler Value

    returnJson response
