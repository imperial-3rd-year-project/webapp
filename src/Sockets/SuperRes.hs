{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE BangPatterns      #-}

module Sockets.SuperRes where

import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Base64 as B
import qualified Data.Vector.Storable as S
import           Data.Word8
import           Grenade
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Static as H
import qualified Data.Array as A
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Sockets.Utils
import           Sockets.Types
import           Control.Concurrent (MVar, modifyMVar_, forkIO, threadDelay)
import           Grenade.Networks.SuperResolution
import           Control.Monad (forM_)

import           Graphics.Image hiding (map)
import           Graphics.Image.ColorSpace
import           Graphics.Image.IO.Formats

import Debug.Trace

processWithSuperRes :: WS.Connection -> SuperResolution -> IO ()
processWithSuperRes site super = do
  (WS.Text bs _) <- (WS.receiveDataMessage site)
  print $ BSL.length bs
  img <- createImage (BSL.toStrict bs)
  let imgYCbCr          = toImageYCbCr $ img
      imgY0             = map (\(PixelYCbCr y _ _ ) -> y ) . concat . toLists $ imgYCbCr
      imgCb             = map (map (\(PixelYCbCr _ cb _) -> cb)) . toLists $ imgYCbCr
      imgCr             = map (map (\(PixelYCbCr _ _ cr) -> cr)) . toLists $ imgYCbCr
      (input, cbs, crs) = (S3D (H.fromList imgY0), imgCb, imgCr)  
      out               = runNet super input
      processed         = processHighResImage out cbs crs
      outputBs          = B.encode (BSL.toStrict (encode OutputJPG [] processed))
  print $ BS.length outputBs
  WS.sendTextData site outputBs

processHighResImage :: S ('D3 672 672 1) -> [[Double]] -> [[Double]] -> Image VS RGBA Double
processHighResImage (S3D m) cbs crs = do 
  let m'  = LA.toLists $ H.extract m      :: [[Double]]
      m'' = map (map PixelX) m'           :: [[Pixel X Double]]
      img = fromLists m''                 :: Image VS X Double

      imgBs  = fromLists $ map (map PixelX) cbs :: Image VS X Double
      imgRs  = fromLists $ map (map PixelX) crs :: Image VS X Double

      imgBs' = resize Bilinear Edge (672, 672) imgBs :: Image VS X Double
      imgRs' = resize Bilinear Edge (672, 672) imgRs :: Image VS X Double

      finalImg = fromImagesX [(LumaYCbCr, img), (CBlueYCbCr, imgBs'), (CRedYCbCr, imgRs')] :: Image VS YCbCr Double
   in toImageRGBA finalImg

createImage :: BS.ByteString -> IO (Image VS RGBA Double)
createImage bs =
  case B.decode bs of
    Left err -> print err >> undefined
    Right decoded -> case decode JPG decoded of
                       Left err -> print err >> undefined
                       Right img -> return img
