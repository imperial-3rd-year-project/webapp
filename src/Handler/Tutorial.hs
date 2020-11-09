{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Tutorial where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

getTutorialR :: Handler Html
getTutorialR = do
    let handlerName = "getTutorialR" :: Text
    defaultLayout $ do
        let (codeTextAreaId, codeFormId, consoleTextAreaId) = textBoxIds
        aDomId <- newIdent
        -- setTitle fot tab title
        
        setTitle "Tutorial"
        $(widgetFile "tutorial")

textBoxIds :: (Text, Text, Text)
textBoxIds = ("js-codeTextAreaId", "js-codeFormId", "js-consoleTextAreaId")

