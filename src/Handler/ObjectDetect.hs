{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ObjectDetect (
    getObjectDetectR,
  ) where

import Import

getObjectDetectR :: Handler Html
getObjectDetectR = do
    defaultLayout $ do
        setTitle "Object Detection"
        $(widgetFile "object-detect")
