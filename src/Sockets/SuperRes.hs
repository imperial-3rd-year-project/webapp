{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}


module Sockets.SuperRes where

import           Control.Concurrent           (readMVar)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Base64       as B
import qualified Data.ByteString.Lazy         as BSL
import           Data.Proxy                   (Proxy)
import qualified Data.Text                    as T
import           Graphics.Image               hiding (map)
import           Grenade
import qualified Network.WebSockets           as WS
import qualified Numeric.LinearAlgebra        as LA
import qualified Numeric.LinearAlgebra.Static as H
import           Sockets.Types

data SuperResProcessor = SuperResProcessor

instance HandlesImage SuperResProcessor where
  handleImage _ stateref socket = do
    WS.Text imageBS _     <- WS.receiveDataMessage socket
    state                 <- readMVar stateref
    (imgY0, imgCb, imgCr) <- either fail return $ preprocessImageSuperRes $ BSL.toStrict imageBS
    WS.sendTextData socket $ postProcessSuperRes imgCb imgCr
                           $ runNet (superres state) imgY0
  matchImageProcessor = matchSuperResProcessor
  getImageMsgFormat   = getSuperResMsgFormat

matchSuperResProcessor :: Proxy SuperResProcessor -> T.Text -> Maybe SuperResProcessor
matchSuperResProcessor _ proc
  | proc == "SUPERRES" = Just SuperResProcessor
  | otherwise          = Nothing

getSuperResMsgFormat :: Proxy SuperResProcessor -> String
getSuperResMsgFormat = const "SUPERRES"

preprocessImageSuperRes :: BS.ByteString -> Either String (S ('D3 224 224 1), [[Double]], [[Double]])
preprocessImageSuperRes bs = preprocess <$> (B.decode bs >>= decode JPG)
  where
    preprocess :: Image VS RGBA Double -> (S ('D3 224 224 1), [[Double]], [[Double]])
    preprocess image = (imgY0, imgCb, imgCr)
      where
        imgYCbCr = toImageYCbCr image
        imgY0    = S3D . H.fromList . map (\(PixelYCbCr y _ _ ) -> y ) . concat . toLists $ imgYCbCr
        imgCb    = map (map (\(PixelYCbCr _ cb _) -> cb)) . toLists $ imgYCbCr
        imgCr    = map (map (\(PixelYCbCr _ _ cr) -> cr)) . toLists $ imgYCbCr

postProcessSuperRes :: [[Double]] -> [[Double]] -> S ('D3 672 672 1) -> BS.ByteString
postProcessSuperRes cbs crs (S3D m) = B.encode (BSL.toStrict (encode OutputJPG [] (toImageRGBA finalImg)))
  where
    m'  = LA.toLists $ H.extract m      :: [[Double]]
    m'' = map (map PixelX) m'           :: [[Pixel X Double]]
    img = fromLists m''                 :: Image VS X Double

    imgBs = fromLists $ map (map PixelX) cbs :: Image VS X Double
    imgRs = fromLists $ map (map PixelX) crs :: Image VS X Double

    imgBs' = resize Bilinear Edge (672, 672) imgBs :: Image VS X Double
    imgRs' = resize Bilinear Edge (672, 672) imgRs :: Image VS X Double

    finalImg = fromImagesX [(LumaYCbCr, img), (CBlueYCbCr, imgBs'), (CRedYCbCr, imgRs')] :: Image VS YCbCr Double
