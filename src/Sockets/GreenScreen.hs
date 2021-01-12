{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}

module Sockets.GreenScreen where

import           Control.Applicative              (liftA2)
import           Control.Concurrent               (MVar, modifyMVar_, readMVar)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL
import           Data.List                        (find)
import           Data.Proxy                       (Proxy)
import qualified Data.Text                        as T
import qualified Data.Vector.Storable             as S
import           Data.Vector.Storable.ByteString  (vectorToByteString)
import           Data.Word8                       (Word8)
import qualified Graphics.Capture.V4L2.Device     as Device
import           Graphics.Display.ConversionUtils (resize)
import           Graphics.Image                   (RGBA, Pixel(..), VS(..), X, Bit(..))
import qualified Graphics.Image                   as I
import           Graphics.Image.Interface
import           Grenade
import           Grenade.Utils.PascalVoc          (DetectedObject)
import qualified Network.WebSockets               as WS
import           Sockets.Types
import           Sockets.Webcam                   (closeWebcam)
import           Sockets.Yolo

data GreenScreenProcessor = GreenScreenProcessor

instance HandlesImage GreenScreenProcessor where
  handleImage _ stateref socket = do
    WS.Binary imageBS <- WS.receiveDataMessage socket
    modifyMVar_ stateref $ \s -> return s { newBg = createImageFromImage (BSL.toStrict imageBS) }
  matchImageProcessor = matchGSProcessor
  getImageMsgFormat   = getGSMsgFormat

instance HandlesCapture GreenScreenProcessor where
  getCallback _ _ _     = return captureWithGS
  matchCaptureProcessor = matchGSProcessor
  getCaptureMsgFormat   = getGSMsgFormat

matchGSProcessor :: Proxy GreenScreenProcessor -> T.Text -> Maybe GreenScreenProcessor
matchGSProcessor _ proc
  | proc == "GS" = Just GreenScreenProcessor
  | otherwise    = Nothing

getGSMsgFormat :: Proxy GreenScreenProcessor -> String
getGSMsgFormat = const "GS"

captureWithGS :: S.Vector Word8 -> MVar ServerState -> WS.Connection -> IO ()
captureWithGS imageV stateref socket = do
  let imageBS = vectorToByteString imageV
  state <- readMVar stateref
  maybe
    -- Record reference background if not done so yet.
    (do modifyMVar_ stateref $ \s -> return s { refBg = Just (createImageFromWebcam imageV) }
        WS.sendBinaryData socket imageBS)
    -- Perform background replacement if already have reference background.
    (\oldbg -> do modifyMVar_ stateref closeWebcam
                  sendGSOutput socket imageBS $ postProcessGreenScreen imageV oldbg (newBg state)
                                              $ runNet (yolo state)
                                              $ preprocessWebcamYolo
                                              $ resize (112, 32) Device.v4l2resolution (416, 416) imageV)
    (refBg state)

createImageFromImage :: BS.ByteString -> Image VS RGBA Double
createImageFromImage bs = I.makeImageR VS (640, 480) maker
  where
    maker :: (Int, Int) -> Pixel RGBA Double
    maker (i, j) = PixelRGBA r g b a
      where
        pixel = (j * 640 + i) * 4
        r = fromIntegral (BS.index bs (pixel + 0)) / 255
        g = fromIntegral (BS.index bs (pixel + 1)) / 255
        b = fromIntegral (BS.index bs (pixel + 2)) / 255
        a = fromIntegral (BS.index bs (pixel + 3)) / 255

createImageFromWebcam :: S.Vector Word8 -> Image VS RGBA Double
createImageFromWebcam v = I.makeImageR VS (640, 480) maker
  where
    maker :: (Int, Int) -> Pixel RGBA Double
    maker (i, j) = PixelRGBA r g b 1.0
      where
        pixel = (j * 640 + i) * 3
        r = fromIntegral (v S.! (pixel + 0)) / 255
        g = fromIntegral (v S.! (pixel + 1)) / 255
        b = fromIntegral (v S.! (pixel + 2)) / 255

postProcessGreenScreen :: S.Vector Word8 -> Image VS RGBA Double -> Image VS RGBA Double
                       -> S ('D3 13 13 125) -> Maybe (Image VS RGBA Double)
postProcessGreenScreen imageV oldbg newbg output
  = fmap (replaceBackground imageV oldbg newbg)
  $ find (\(_, _, _, _, _, item) -> item == "person")
  $ postProcessYolo False output
 
sendGSOutput :: WS.Connection -> BS.ByteString -> Maybe (Image VS RGBA Double) -> IO ()
sendGSOutput socket imageBS Nothing = do
  WS.sendBinaryData socket imageBS
  WS.sendTextData socket ("END GS" :: T.Text)
sendGSOutput socket _ (Just image)  = do
  WS.sendBinaryData socket $ fst $ BS.unfoldrN (640 * 480 * 3) gen 0
  WS.sendTextData socket ("END GS" :: T.Text)
  where
    gen :: Int -> Maybe (Word8, Int)
    gen i = Just (value, i + 1)
      where
        (pos, channel)    = divMod i 3
        (x, y)            = divMod pos 640
        PixelRGBA r g b _ = I.index image (y, x)
        value             = floor $ ([r, g, b] !! channel) * 255


replaceBackground :: S.Vector Word8 -> Image VS RGBA Double -> Image VS RGBA Double
                  -> DetectedObject -> Image VS RGBA Double
replaceBackground imageV oldbg newbg (l, r, t, b, _, _) = I.izipWith pickPixels mask img
  where
    -- The box that Yolo gives seems to be smaller than the person,
    -- so as an approximation, we vertically extend the box by 20% in
    -- both directions. This means that in particular, features such as
    -- the top of the person's head doesn't affect the mean difference.
    (extendH, extendV) = (0, (b - t) `div` 5)
    (l', r') = (max 0 (l + 112 - extendH), min (r + 112 + extendH) 640)
    (t', b') = (max 0 (t + 32  - extendV), min (b + 32  + extendV) 480)

    -- Calculate differences in pixels and get stats on those.
    !img      = createImageFromWebcam imageV
    !diffs    = I.applyFilter (I.gaussianBlur 15) -- Blur helps remove noise.
              $ I.zipWith (liftPx2 (\x y -> abs $ x - y)) img oldbg
    !(!diffMean, !diffVariance) = getStats l' r' t' b' diffs

    -- Construct mask for blending foreground and background.
    !structD  = I.fromLists [[0,1,0],[0,1,0],[1,1,1]]
    !faceMask = I.open structD                     -- Try removing noise with morphology.
              $ I.crop (l', t') (r' - l', b' - t') -- Crop to detected face.
              $ I.toImageBinaryUsing markFGPixel diffs
    !baseMask = I.makeImageR VS (640, 480) (const $ PixelX $ I.Bit 0)
    !mask     = I.superimpose (l', t') faceMask baseMask

    markFGPixel :: Pixel RGBA Double -> Bool
    markFGPixel diffPixel = or $ liftA2 (>) (diffPixel - diffMean) ((1.5 *) . sqrt <$> diffVariance)

    pickPixels :: (Int, Int) -> Pixel X Bit -> Pixel RGBA Double -> Pixel RGBA Double
    pickPixels (x, y) maskPixel ogPixel
      | I.isOn maskPixel = ogPixel
      | otherwise        = I.index newbg (x, y)

-- | Given a hip image, return the mean and variance of the pixels in the image outside
--   given box.
getStats :: Array arr RGBA Double
         => Int -> Int -> Int -> Int -> Image arr RGBA Double -> (Pixel RGBA Double, Pixel RGBA Double)
getStats left right top bottom img = (means, vars)
  where
    means = (/ pixelsOutside) <$> sums
    vars  = liftA2 (-) meansSquared (means * means)

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
