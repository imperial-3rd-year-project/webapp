{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ImageClass (
    getImageClassR,
  ) where

import Import

getImageClassR :: Handler Html
getImageClassR = do
    defaultLayout $ do
        let (canvasId, devicesId) = canvasIds
        setTitle "Image Classification"
        $(widgetFile "image-class")

canvasIds :: (Text, Text)
canvasIds = ("js-canvasId", "js-devicesId")
