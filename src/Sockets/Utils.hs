module Sockets.Utils where

import qualified Network.WebSockets as WS
import qualified Data.Text as T

sendErr :: String -> WS.Connection -> IO ()
sendErr errMsg conn = WS.sendTextData conn (T.pack ("ERR " ++ errMsg))

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err Nothing  = Left err
maybeToEither _   (Just x) = Right x 
