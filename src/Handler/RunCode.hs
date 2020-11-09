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

newtype UserCode = UserCode {unUserCode :: String}
  deriving Show
  deriving newtype (FromJSON, ToJSON)

postRunCodeR :: Handler Value
postRunCodeR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    code <- requireCheckJsonBody :: Handler UserCode
    -- liftIO $ putStrLn $ (unUserCode code)
    -- fExpr is a Haskell code supplied by your user as a String
    -- let fExpr = "2 + 2"
    let fExpr = (unUserCode code)
    -- Create an interpreter that runs fExpr
    r <- liftIO $ runInterpreter $ do
            setImports ["Prelude"]
            interpret fExpr (as :: Int)
    -- run it and get an interface to the function
    res <- case r of
        Left err -> return $ toJSON $ "Ups... " ++ (show err)
        Right f -> return $ toJSON $ (show f)
        -- Right f -> do 
        --     let res = (show f) :: String
        --     return . return $ toJSON $ res
        -- Right f  -> do
        --     liftIO $ print $ f [True, False]
        --     liftIO $ print $ f [True, True]  

    return res


