{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Tutorial where

import Import
import Text.Julius (RawJS (..))
import System.IO as IO
import System.Directory ( getCurrentDirectory )

import Data.Text as T



getTutorialR :: Handler Html
getTutorialR = do
    defaultLayout $ do
        let (codeTextAreaId, codeFormId, consoleTextAreaId) = textBoxIds
        exampleCode <- liftIO $ getExampleCode

        setTitle "Tutorial"
        $(widgetFile "tutorial")

textBoxIds :: (Text, Text, Text)
textBoxIds = ("js-codeTextAreaId", "js-codeFormId", "js-consoleTextAreaId")

getExampleCode :: IO Text 
getExampleCode = do 
    cwd <- liftIO $ getCurrentDirectory
    IO.readFile (cwd ++ "/grenade-tutorials/src/circle.hs") >>= return . T.pack
   

