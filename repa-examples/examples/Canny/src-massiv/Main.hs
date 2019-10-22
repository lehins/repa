{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

-- | Canny edge detector.
--
--   NOTE: for best performance this needs to be compiled with the following GHC options:
--         -fllvm -optlo-O3 -Odph -fno-liberate-case
--         -funfolding-use-threshold100 -funfolding-keeness-factor100
--
import Control.Monad
import Control.DeepSeq
import Data.Array.Repa.IO.Timing
import Data.Int
import Data.List
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Data.Massiv.Array.Stencil.Unsafe as A
import Data.Massiv.Array.IO as A
import Data.Word
import Debug.Trace
import GHC.Exts
import Graphics.ColorSpace
import Prelude hiding (compare)
import qualified Prelude as P
import System.Environment



-- Constants ------------------------------------------------------------------
orientUndef     = 0     :: Word8
orientPosDiag   = 64    :: Word8
orientVert      = 128   :: Word8
orientNegDiag   = 192   :: Word8
orientHoriz     = 255   :: Word8

edgeNone   = 0     :: Pixel Y Word8
edgeWeak   = 128   :: Pixel Y Word8
edgeStrong = 255   :: Pixel Y Word8


-- Main routine ---------------------------------------------------------------
main
 = do   args    <- getArgs
        case args of
         [fileIn, fileOut]
           -> run 0 50 100 fileIn fileOut

         [loops, threshLow, threshHigh, fileIn, fileOut]
           -> run (P.read loops) (P.read threshLow) (P.read threshHigh) fileIn fileOut

         _ -> putStrLn
           $ concat [ "massiv-canny [<loops::Int> <threshLow::Int> <threshHigh::Int>]"
                    , " <fileIn.bmp> <fileOut.bmp>" ]

run :: Int -> Float -> Float -> FilePath -> FilePath -> IO ()
run loops threshLow threshHigh fileIn fileOut
 = do   arrInput <- setComp Par <$> readImageAuto fileIn

        (arrResult, tTotal)
         <- time $ process loops threshLow threshHigh arrInput

        when (loops >= 1)
         $ putStrLn $ "\nTOTAL\n"

        putStr $ prettyTime tTotal

        writeImage fileOut arrResult

process :: Int -> Float -> Float -> Image S RGB Word8 -> IO (Image S Y Word8)
process loops threshLow threshHigh arrInput = do
        arrGrey         <- timeStage loops "toGreyScale"
                           (toGreyScale arrInput)

        arrBluredX      <- timeStage loops "blurX"
                           (blurSepX arrGrey)

        arrBlured       <- timeStage loops "blurY"
                           (blurSepY arrBluredX)


        arrDX           <- timeStage loops "diffX"
                           (gradientX arrBlured)

        arrDY           <- timeStage loops "diffY"
                           (gradientY arrBlured)

        arrMagOrient    <- timeStage loops "magOrient"
                           (gradientMagOrient threshLow arrDX arrDY)

        arrSuppress     <- timeStage loops "suppress"
                           (suppress threshLow threshHigh arrMagOrient)

        arrStrong       <- timeStage loops "select"
                           (selectStrong arrSuppress)

        arrEdges        <- timeStage loops "wildfire"
                        $  wildfire arrSuppress arrStrong

        return arrEdges


-- | Wrapper to time each stage of the algorithm.
timeStage
        :: NFData a
        => Int
        -> String
        -> IO a
        -> IO a

timeStage loops name fn = do
        let burn !n
             = do !arr  <- fn
                  if n <= 1 then return arr
                    else burn (n - 1)

        traceEventIO $ "**** Stage " P.++ name P.++ " begin."

        (arrResult, t)
         <- time $ do  !arrResult' <- burn loops
                       return arrResult'

        traceEventIO $ "**** Stage " P.++ name P.++ " end."

        when (loops >= 1)
         $ putStr       $  name P.++ "\n"
                        P.++ unlines [ "  " P.++ l | l <- lines $ prettyTime t ]

        return arrResult
{-# NOINLINE timeStage #-}


-------------------------------------------------------------------------------
-- | RGB to greyscale conversion.
toGreyScale :: Image S RGB Word8 -> IO (Image S Y Float)
toGreyScale = pure . compute . A.map ((*255) . toPixelYF)
{-# NOINLINE toGreyScale #-}

toPixelYF :: Pixel RGB Word8 -> Pixel Y Float
toPixelYF (PixelRGB r g b) =
  let !r' = fromIntegral r / 255
      !g' = fromIntegral g / 255
      !b' = fromIntegral b / 255
   in PixelY (r' * 0.3 + g' * 0.59 + b' * 0.11)
{-# INLINE toPixelYF #-}

-- | Separable Gaussian blur in the X direction.
blurSepX :: Image S Y Float -> IO (Image S Y Float)
blurSepX =
  pure . compute .
  mapStencil
    Edge
    (makeStencil (Sz2 1 5) (0 :. 2) $ \get ->
       get (0 :. -2)     +
       get (0 :. -1) * 4 +
       get (0 :.  0) * 6 +
       get (0 :.  1) * 4 +
       get (0 :.  2)
    )
{-# NOINLINE blurSepX #-}

-- gaussianX :: Num e => Stencil Ix2 e e
-- gaussianX =
--   makeCorrelationStencil (Sz2 1 5) (0 :. 2) $ \get ->
--     get (0 :. -2) 1 .
--     get (0 :. -1) 4 .
--     get (0 :.  0) 6 .
--     get (0 :.  1) 4 .
--     get (0 :.  2) 1
-- {-# INLINE gaussianX #-}

-- gaussianY :: Num e => Stencil Ix2 e e
-- gaussianY =
--   makeCorrelationStencil (Sz2 5 1) (2 :. 0) $ \get ->
--     get (-2 :. 0) 1 .
--     get (-1 :. 0) 4 .
--     get ( 0 :. 0) 6 .
--     get ( 1 :. 0) 4 .
--     get ( 2 :. 0) 1
-- {-# INLINE gaussianY #-}


-- | Separable Gaussian blur in the Y direction.
blurSepY :: Image S Y Float -> IO (Image S Y Float)
blurSepY =
  pure . compute . fmap (/256) .
  mapStencil
    Edge
    (makeStencil (Sz2 5 1) (2 :. 0) $ \get ->
       get (-2 :. 0)     +
       get (-1 :. 0) * 4 +
       get ( 0 :. 0) * 6 +
       get ( 1 :. 0) * 4 +
       get ( 2 :. 0)
    )
{-# NOINLINE blurSepY #-}

-- -- | Compute gradient in the X direction.
-- gradientX :: Image S Y Float -> Image S Y Float
-- gradientX =
--   compute .
--   mapStencil
--     Edge
--     (makeStencil (Sz 3) (1 :. 1) $
--     \ f -> f (-1 :.  1)     +
--            f ( 0 :.  1) * 2 +
--            f ( 1 :.  1)     -
--            f (-1 :. -1)     -
--            f ( 0 :. -1) * 2 -
--            f ( 1 :. -1)
--     )
-- {-# INLINE gradientX #-}


-- -- | Compute gradient in the Y direction.
-- gradientY :: Image S Y Float -> Image S Y Float
-- gradientY =
--   compute .
--   mapStencil
--     Edge
--     (makeStencil (Sz 3) (1 :. 1) $
--      \ f -> f ( 1 :. -1)     +
--             f ( 1 :.  0) * 2 +
--             f ( 1 :.  1)     -
--             f (-1 :. -1)     -
--             f (-1 :.  0) * 2 -
--             f (-1 :.  1)
--     )
-- {-# INLINE gradientY #-}

-- | Compute gradient in the X direction.
gradientX :: Image S Y Float -> IO (Image S Y Float)
gradientX = pure . compute . mapStencil Edge sobelX
{-# INLINE gradientX #-}


-- | Compute gradient in the Y direction.
gradientY :: Image S Y Float -> IO (Image S Y Float)
gradientY = pure . compute . mapStencil Edge sobelY

-- sobelX :: (Default e, Num e) => Stencil Ix2 e e
-- sobelX =
--   makeStencil (Sz 3) (1 :. 1) $
--   \ f -> f (-1 :. -1) * (-1) +
--          f ( 0 :. -1) * (-2) +
--          f ( 1 :. -1) * (-1) +
--          f (-1 :.  1)        +
--          f ( 0 :.  1) *    2 +
--          f ( 1 :.  1)
-- {-# INLINE sobelX #-}


-- sobelY :: (Default e, Num e) => Stencil Ix2 e e
-- sobelY =
--   makeStencil (Sz 3) (1 :. 1) $
--   \ f -> f (-1 :. -1) * (-1) +
--          f (-1 :.  0) * (-2) +
--          f (-1 :.  1) * (-1) +
--          f ( 1 :. -1)        +
--          f ( 1 :.  0) *    2 +
--          f ( 1 :.  1)
-- {-# INLINE sobelY #-}

sobelX :: Num e => Stencil Ix2 e e
sobelX = makeCorrelationStencil (Sz 3) (1 :. 1) $ \ f -> f (-1 :. -1) (-1) .
                                                         f ( 0 :. -1) (-2) .
                                                         f ( 1 :. -1) (-1) .
                                                         f (-1 :.  1)   1  .
                                                         f ( 0 :.  1)   2  .
                                                         f ( 1 :.  1)   1
{-# INLINE sobelX #-}


sobelY :: Num e => Stencil Ix2 e e
sobelY = makeCorrelationStencil (Sz 3) (1 :. 1) $ \ f -> f (-1 :. -1) (-1) .
                                                         f (-1 :.  0) (-2) .
                                                         f (-1 :.  1) (-1) .
                                                         f ( 1 :. -1)   1  .
                                                         f ( 1 :.  0)   2  .
                                                         f ( 1 :.  1)   1
{-# INLINE sobelY #-}


-- | Classify the magnitude and orientation of the vector gradient.
gradientMagOrient
        :: Float -> Image S Y Float -> Image S Y Float -> IO (Array S Ix2 Float, Array S Ix2 Word8)
gradientMagOrient !threshLow !dX !dY = pure (mag, orient)

 where  !mag = compute $ A.zipWith magnitude' dX dY
        !orient = compute $ A.zipWith orientation dX dY

        magnitude' :: Pixel Y Float -> Pixel Y Float -> Float
        magnitude' (PixelY x) (PixelY y) = sqrt (x * x + y * y)
        {-# INLINE magnitude' #-}

        {-# INLINE orientation #-}
        orientation :: Pixel Y Float -> Pixel Y Float -> Word8
        orientation (PixelY x) (PixelY y)

         -- Don't bother computing orientation if vector is below threshold.
         | x >= negate threshLow, x < threshLow
         , y >= negate threshLow, y < threshLow
         = orientUndef

         | otherwise
         = let  -- Determine the angle of the vector and rotate it around a bit
                -- to make the segments easier to classify.
                !d      = atan2 y x
                !dRot   = (d - (pi/8)) * (4/pi)

                -- Normalise angle to beween 0..8
                !dNorm  = if dRot < 0 then dRot + 8 else dRot

                -- Doing explicit tests seems to be faster than using the FP floor function.
           in fromIntegral
               $ I# (if dNorm >= 4
                     then if dNorm >= 6
                          then if dNorm >= 7
                                then 255#               -- 7
                                else 192#               -- 6

                          else if dNorm >= 5
                                then 128#               -- 5
                                else 64#                -- 4

                     else if dNorm >= 2
                        then if dNorm >= 3
                                then 255#               -- 3
                                else 192#               -- 2

                        else if dNorm >= 1
                                then 128#               -- 1
                                else 64#)               -- 0
{-# NOINLINE gradientMagOrient #-}


-- | Suppress pixels that are not local maxima, and use the magnitude to classify maxima
--   into strong and weak (potential) edges.
suppress :: Float -> Float -> (Array S Ix2 Float, Array S Ix2 Word8) -> IO (Image S Y Word8)
suppress !threshLow !threshHigh (!dMag, !dOrient)
  = pure
  $ compute
  $ mapStencil (Fill 0) (makeUnsafeStencil 3 1 comparePts) dMag
 where  {-# INLINE comparePts #-}
        comparePts !ix getMag
         | o == orientUndef     = edgeNone
         | o == orientHoriz     = isMax (getMag ( 0 :. -1)) (getMag ( 0 :.  1))
         | o == orientVert      = isMax (getMag (-1 :.  0)) (getMag ( 1 :.  0))
         | o == orientNegDiag   = isMax (getMag (-1 :. 1)) (getMag ( 1 :.  -1))
         | o == orientPosDiag   = isMax (getMag (-1 :. -1)) (getMag ( 1 :.  1))
         -- | o == orientNegDiag   = isMax (getMag (-1 :. -1)) (getMag ( 1 :.  1)) --?????
         -- | o == orientPosDiag   = isMax (getMag (-1 :.  1)) (getMag ( 1 :. -1)) --?????
         | otherwise            = edgeNone

         where
          !o = unsafeIndex dOrient ix
          !m = getMag (0 :. 0)

          {-# INLINE isMax #-}
          isMax !intensity1 !intensity2
            | m < threshLow     = edgeNone
            | m < intensity1    = edgeNone
            | m < intensity2    = edgeNone
            | m < threshHigh    = edgeWeak
            | otherwise         = edgeStrong
{-# NOINLINE suppress #-}


-- | Select indices of strong edges.
selectStrong :: Image S Y Word8 -> IO (Array S Ix1 Ix1)
selectStrong =
  pure . compute .
  imapMaybeS
    (\ !ix !e ->
       if e == edgeStrong
         then Just ix
         else Nothing) . flatten
{-# NOINLINE selectStrong #-}


-- | Trace out strong edges in the final image.
--   Also trace out weak edges that are connected to strong edges.
wildfire ::
     Image S Y Word8 -- ^ Image with strong and weak edges set.
  -> Array S Ix1 Ix1 -- ^ Array containing indices of strong edges.
  -> IO (Image S Y Word8)
wildfire img vStrong = do
  vStrong' <- A.thaw vStrong
  -- Stack of image indices we still need to consider.
  vStack <- A.unsafeLinearGrow vStrong' (Sz lenImg)
  -- Burn in new edges.
  vImg <- A.new sz
  burn vImg vStack (unSz (size vStrong))
  unsafeFreeze (getComp img) vImg
  where
    !sz = A.size img
    !lenImg = totalElem sz
    burn ::
         MArray RealWorld S Ix2 (Pixel Y Word8)
      -> MArray RealWorld S Ix1 Ix1
      -> Int
      -> IO ()
    burn !vImg !vStack = go
      where
        push !ix !t =
          case indexM img ix of
            Nothing -> pure t
            Just xSrc -> do
              xDst <- unsafeRead vImg ix
              if xDst == edgeNone && xSrc == edgeWeak
                -- If this ix is weak in the source then set it to strong in the result
                -- and push the ix onto the stack.
                then do
                  unsafeWrite vStack t $ toLinearIndex sz ix
                  return (t + 1)
                else return t
        {-# INLINE push #-}
        go !top
          | top == 0 = return ()
          | otherwise = do
            let !top' = top - 1
            ix@(y :. x) <- fromLinearIndex sz <$> readM vStack top'
            unsafeWrite vImg ix (edgeStrong)
            push (y - 1 :. x - 1) top' >>= push (y - 1 :. x) >>=
              push (y - 1 :. x + 1) >>=
              push (y :. x - 1) >>=
              push (y :. x + 1) >>=
              push (y + 1 :. x - 1) >>=
              push (y + 1 :. x) >>=
              push (y + 1 :. x + 1) >>=
              go
{-# NOINLINE wildfire #-}
