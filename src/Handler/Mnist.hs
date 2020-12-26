{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Mnist where

import Import

getMnistR :: Handler Html
getMnistR = do
    defaultLayout $ do
        let (mnistDataId, canvasId) = canvasIds
        setTitle "MNIST Demo"
        $(widgetFile "mnist-demo")

canvasIds :: (Text, Text)
canvasIds = ("js-mnistDataId", "js-canvasId")
