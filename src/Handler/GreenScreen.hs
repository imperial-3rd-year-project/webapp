{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.GreenScreen (
    getGreenScreenR,
  ) where

import Import

getGreenScreenR :: Handler Html
getGreenScreenR = do
    defaultLayout $ do
        setTitle "Green Screen"
        $(widgetFile "green-screen")
