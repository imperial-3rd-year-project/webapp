{-# LANGUAGE OverloadedStrings
           , DeriveGeneric
           , GeneralisedNewtypeDeriving
           , DerivingStrategies
#-}
module Handler.RunCode where

import Import (Handler, requireCheckJsonBody, Text, returnJson)
import Data.Aeson
import Control.Monad
import Language.Haskell.Interpreter
import System.IO.Temp 
import System.Process
import GHC.IO.Handle
import System.FilePath.Posix
import System.Directory ( getCurrentDirectory )

-- import           Control.Monad
-- import           Control.Monad.Random
-- import           GHC.TypeLits

-- import qualified Numeric.LinearAlgebra.Static as SA

-- import           Grenade
import Data.Maybe

newtype UserCode = UserCode {unUserCode :: String}
  deriving Show
  deriving newtype (FromJSON, ToJSON)

postRunCodeR :: Handler Value
postRunCodeR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    code <- requireCheckJsonBody :: Handler UserCode
    let contents = (unUserCode code)
    cwd <- liftIO $ getCurrentDirectory
    let cwd' = cwd ++ "/grenade-tutorials"
    let tmpFile = cwd' ++ "/src/circle-user.hs"
    liftIO $ print tmpFile
    liftIO $ writeFile tmpFile contents
    (_, hout, _ , _) <- liftIO $ createProcess (shell ("stack run circle-user")){ cwd = Just cwd', std_out = CreatePipe}
    res <- do
        case hout of 
            (Just h) -> liftIO $ hGetContents h
            (Nothing) -> error "ERROR" 

    return $ toJSON $ res

