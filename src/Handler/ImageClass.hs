{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ImageClass where

import Import

getImageClassR :: Handler Html
getImageClassR = do
    defaultLayout $ do
        let (mnistDataId, canvasId) = canvasIds
        setTitle "Image Classification"
        $(widgetFile "image-class")

canvasIds :: (Text, Text)
canvasIds = ("js-mnistDataId", "js-canvasId")
