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

-- import           Control.Monad
-- import           Control.Monad.Random
-- import           GHC.TypeLits

-- import qualified Numeric.LinearAlgebra.Static as SA

-- import           Grenade


newtype UserCode = UserCode {unUserCode :: String}
  deriving Show
  deriving newtype (FromJSON, ToJSON)

postRunCodeR :: Handler Value
postRunCodeR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    code <- requireCheckJsonBody :: Handler UserCode
    let contents = (unUserCode code)
    tmpFile <- liftIO $ writeSystemTempFile "tmpTut.hs" contents
    let (dir, file) = splitFileName tmpFile
    liftIO $ print tmpFile
    (_, hout, _ , _) <- liftIO $ createProcess (shell ("runghc "++ file)){ cwd = Just dir, std_out = CreatePipe}
    res <- do
        case hout of 
            (Just h) -> liftIO $ hGetContents h
            (Nothing) -> error "ERROR" 

    return $ toJSON $ res




