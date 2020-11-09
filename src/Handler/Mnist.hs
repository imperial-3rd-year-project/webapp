{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Mnist where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getMnistR :: Handler Html
getMnistR = do
    let handlerName = "getMnistR" :: Text
    defaultLayout $ do
        let (mnistDataId, canvasId) = canvasIds
        aDomId <- newIdent
        -- setTitle fot tab title
        
        setTitle "MNIST Demo"
        $(widgetFile "mnist-demo")

canvasIds :: (Text, Text)
canvasIds = ("js-mnistDataId", "js-canvasId")
