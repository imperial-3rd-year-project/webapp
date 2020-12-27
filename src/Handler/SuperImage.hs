{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.SuperImage (
    getSuperImageR,
  ) where

import Import

getSuperImageR :: Handler Html
getSuperImageR = do
    defaultLayout $ do
        setTitle "Image Super-resolution"
        $(widgetFile "super-image")
