{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}

module Sockets.GreenScreen where

import           Control.Applicative              (liftA2)
import           Control.Concurrent               (MVar, modifyMVar_, readMVar)
import           Control.Monad                    (when)
import qualified Data.Vector.Storable             as S
import           Data.Word8
import           Data.Maybe                       (isNothing, isJust)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL
import           Graphics.Capture.Class
import qualified Graphics.Capture.V4L2.Device     as Device
import           Graphics.Display.ConversionUtils (resize)
import           Grenade.Utils.PascalVoc
import qualified Network.WebSockets               as WS
import           Sockets.Types
import           Sockets.Utils
import           Sockets.Yolo
import qualified Data.Text                        as T

import           Graphics.Image                   as I hiding (resize, or)
import           Graphics.Image.Interface              hiding (map)

enableGreenScreen :: WS.Connection -> MVar ServerState -> IO ()
enableGreenScreen _ state = modifyMVar_ state $ \s -> return s { imgProc = Just GreenScreen }

updateNewBackground :: WS.Connection -> MVar ServerState -> IO ()
updateNewBackground conn state = do
  WS.Binary bs <- WS.receiveDataMessage conn
  modifyMVar_ state $ \s -> return s { newBg = createImageFromBS' $ BSL.toStrict bs }

replaceBackground :: MVar ServerState -> S.Vector Word8 -> BS.ByteString -> IO ()
replaceBackground mstate imgVec imgBs = do
  state <- readMVar mstate
  let Just (WebsiteConn conn') = conn state
      mbg   = refBg state
      newbg = newBg state
  if isNothing mbg
    then do
      -- Initially we will capture the background
      modifyMVar_ mstate $ \s -> return s { refBg = Just (createImageFromBS imgBs), imgProc = Nothing }
      WS.sendBinaryData conn' imgBs
    else do
      -- The second time the capture is invoked, we can perform analysis on it
      let Just oldbg = mbg
          centered   = resize (112, 32) Device.v4l2resolution (416, 416) imgVec
          input      = preprocessYolo' centered
          yolo'      = yolo state
      modifyMVar_ mstate closeWebcam -- close the webcam
      output <- runYolo input yolo'
      let filtered = filter (\(_, _, _, _, _, item) -> item == "person") output
      if null filtered 
        then WS.sendBinaryData conn' imgBs >> putStrLn "No people detected."
        else do
          let (l, r, t, b, _, n) = getPrincipalBox filtered
              -- The box that Yolo gives seems to be smaller than the person,
              -- so as an approximation, we vertically extend the box by 20% in
              -- both directions. This means that in particular, features such as
              -- the top of the person's head doesn't affect the mean difference.
              (extendH, extendV) = (0, (b - t) `div` 5)
              (l', r') = (max 0 (l + 112 - extendH), min (r + 112 + extendH) 640)
              (t', b') = (max 0 (t + 32  - extendV), min (b + 32  + extendV) 480)

              -- Calculate differences in pixels and get stats on those.
              !img      = createImageFromBS imgBs
              !diffs    = applyFilter (gaussianBlur 15) -- Blur helps remove noise.
                        $ I.zipWith (liftPx2 (\x y -> abs $ x - y)) img oldbg
              !(!diffMean, diffVariance) = getStats l' r' t' b' diffs

              -- Construct mask for blending foreground and background.
              !structD  = fromLists [[0,1,0],[0,1,0],[1,1,1]] :: Image VS X Bit 
              !faceMask = open structD                     -- Try removing noise with morphology.
                        $ crop (l', t') (r' - l', b' - t') -- Crop to detected face.
                        $ toImageBinaryUsing markFGPixel diffs
              !baseMask = makeImageR VS (640, 480) (const $ PixelX $ Bit 0)
              !mask     = superimpose (l', t') faceMask baseMask

              markFGPixel diffPixel = or $ liftA2 (>) (diffPixel - diffMean) ((1.5 *) . sqrt <$> diffVariance) --(PixelRGBA 0.2 0.2 0.2 1.0)
              pickPixels (x, y) maskPixel ogPixel
                | isOn maskPixel = ogPixel
                | otherwise      = I.index newbg (x, y)

          WS.sendBinaryData conn' $ fromImage $ I.izipWith pickPixels mask img
          -- Send the bounding box data (debugging purposes)
          WS.sendTextData conn' ("BEGIN YOLO" :: T.Text)
          WS.sendTextData conn' (T.pack n)
          WS.sendTextData conn' (T.pack $ show l')
          WS.sendTextData conn' (T.pack $ show r')
          WS.sendTextData conn' (T.pack $ show t')
          WS.sendTextData conn' (T.pack $ show b')
          WS.sendTextData conn' ("END YOLO" :: T.Text)

createImageFromBS :: BS.ByteString -> Image VS RGBA Double
createImageFromBS bs = makeImageR VS (640, 480) maker
  where
    maker :: (Int, Int) -> Pixel RGBA Double
    maker (i, j) = PixelRGBA r g b 1.0
      where
        pixel = (j * 640 + i) * 3
        r = fromIntegral (BS.index bs (pixel + 0)) / 255
        g = fromIntegral (BS.index bs (pixel + 1)) / 255
        b = fromIntegral (BS.index bs (pixel + 2)) / 255

createImageFromBS' :: BS.ByteString -> Image VS RGBA Double
createImageFromBS' bs = makeImageR VS (640, 480) maker
  where
    maker :: (Int, Int) -> Pixel RGBA Double
    maker (i, j) = PixelRGBA r g b a
      where
        pixel = (j * 640 + i) * 4
        r = fromIntegral (BS.index bs (pixel + 0)) / 255
        g = fromIntegral (BS.index bs (pixel + 1)) / 255
        b = fromIntegral (BS.index bs (pixel + 2)) / 255
        a = fromIntegral (BS.index bs (pixel + 3)) / 255

fromBinImage :: Image VS X Bit -> BS.ByteString
fromBinImage img = fst $ BS.unfoldrN (640 * 480 * 3) maker 0
  where
    maker :: Int -> Maybe (Word8, Int)
    maker i = Just (value, i + 1)
      where
        (pos, _)         = divMod i 3
        (x, y)           = divMod pos 640
        PixelX (Bit val) = index img (y, x)
        value            = val * 255

fromImage :: Image VS RGBA Double -> BS.ByteString
fromImage img = fst $ BS.unfoldrN (640 * 480 * 3) maker 0
  where
    maker :: Int -> Maybe (Word8, Int)
    maker i = Just (value, i + 1)
      where
        (pos, channel) = divMod i 3
        (x, y)         = divMod pos 640
        PixelRGBA r g b _ = index img (y, x)
        value = floor $ ([r, g, b] !! channel) * 255

-- Given a hip image, return the mean and variance of the pixels in the image outside the given box.
getStats :: Array arr RGBA Double
         => Int -> Int -> Int -> Int -> Image arr RGBA Double -> (Pixel RGBA Double, Pixel RGBA Double)
getStats left right top bottom img = (means, vars)
  where
    means = (/ pixelsOutside) <$> sums
    vars  = liftPx2 (-) meansSquared (means * means)

    sums          = sumOutside img
    sumsSquared   = sumOutside $ I.zipWith (*) img img
    meansSquared  = (/ pixelsOutside) <$> sumsSquared

    pixelsOutside = fromIntegral $ 640 * 480 - (width * height)
    width         = min 640 $ max 0 $ right - left
    height        = min 480 $ max 0 $ bottom - top

    sumOutside = foldIx accIfOutside (PixelRGBA 0 0 0 0)

    accIfOutside acc (x, y) pixel
      | left <= x && x <= right && top <= y && y <= bottom = acc
      | otherwise                                          = acc + pixel

getPrincipalBox :: [DetectedObject] -> DetectedObject
getPrincipalBox = head
