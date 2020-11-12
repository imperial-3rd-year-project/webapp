{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Tutorial where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import System.IO as IO
import System.IO.Unsafe
import Control.Monad

getTutorialR :: Handler Html
getTutorialR = do
    let handlerName = "getTutorialR" :: Text

    defaultLayout $ do
        let (codeTextAreaId, codeFormId, consoleTextAreaId) = textBoxIds
        exampleCode <- liftIO $ getExampleCode

        aDomId <- newIdent
        -- setTitle fot tab title
        
        setTitle "Tutorial"
        $(widgetFile "tutorial")

textBoxIds :: (Text, Text, Text)
textBoxIds = ("js-codeTextAreaId", "js-codeFormId", "js-consoleTextAreaId")

getExampleCode :: IO Text 
getExampleCode = do 
    contents <- IO.readFile "/Users/gabymarfani/Desktop/imperial/year_3/group_project/grenade/examples/main/circle.hs" 
    return $ pack contents
    
    

