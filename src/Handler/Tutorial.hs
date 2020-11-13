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
import System.Directory ( getCurrentDirectory )

import System.IO.Temp 
import System.Process
import GHC.IO.Handle
import System.FilePath.Posix



getTutorialR :: Handler Html
getTutorialR = do
    let handlerName = "getTutorialR" :: Text

    defaultLayout $ do
        let (codeTextAreaId, codeFormId, consoleTextAreaId) = textBoxIds
        App {tutorialDir=tmpDir} <- getYesod
        exampleCode <- liftIO $ getExampleCode tmpDir

        aDomId <- newIdent
        -- setTitle fot tab title
        
        setTitle "Tutorial"
        $(widgetFile "tutorial")

textBoxIds :: (Text, Text, Text)
textBoxIds = ("js-codeTextAreaId", "js-codeFormId", "js-consoleTextAreaId")

getExampleCode :: IO Text 
getExampleCode = do 
    cwd <- liftIO $ getCurrentDirectory
    contents <- IO.readFile (cwd ++ "/grenade-tutorials/src/circle.hs")
   

