module Sockets.Utils where

import qualified Network.WebSockets as WS
import qualified Data.Text as T

sendErr :: String -> WS.Connection -> IO ()
sendErr errMsg conn = WS.sendTextData conn (T.pack ("ERR " ++ errMsg))


