-- |
-- Module      : Data.Vector.Unboxed
-- Copyright   : (c) Kwang Yul Seo 2016
-- License     : BSD-style
--
-- Maintainer  : Kwang Yul Seo <kwangyul.seo@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Perceptual image hash calculation tool based on algorithm descibed in
-- Block Mean Value Based Image Perceptual Hashing by Bian Yang, Fan Gu and Xiamu Niu.
module Data.Blockhash
  ( Image(..)
  , Hash(..)
  , hammingDistance
  , blockhash
  , Method(..)
  ) where

import Control.Monad (forM_)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits (popCount, xor)
import Data.Fixed (mod')
import Data.Char (digitToInt, intToDigit)
import Data.STRef (newSTRef, modifySTRef, readSTRef)
import qualified Data.Vector.Algorithms.Intro as VA
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)

newtype Hash = Hash { unHash :: Vector Char }

instance Show Hash where
  show (Hash h) = V.toList h

data Image = Image
  { imageWidth :: Int
  , imageHeight :: Int
  , imagePixels :: Vector Word8
  } deriving (Show)

-- | Calculate the hamming distance for two hashes in hex format
hammingDistance :: Hash -> Hash -> Int
hammingDistance (Hash h1) (Hash h2) =
  V.sum $ V.map popCount $ V.zipWith xor (V.map digitToInt h1) (V.map digitToInt h2)

data Method =
    Precise -- ^ Precise method puts weighted pixel values to blocks according to pixel
            -- area falling within a given block and provides more accurate results
            -- in case width and height are not divisible by the number of bits.
  | Quick -- ^ Quick method uses rounded block sizes and is less accurate in case image
          -- width and height are not divisible by the number of bits.
  deriving (Eq, Show, Enum, Bounded)

-- | Calculate perceptual hash for an RGBA image
blockhash :: Image -- ^ RGBA image data
          -> Int -- ^ The number of blocks to divide the image by horizontally and vertically
          -> Method -- ^ Method
          -> Hash -- ^ The result
blockhash image bits method =
  let pixels = imagePixels image
      width = imageWidth image
      height = imageHeight image
      f = case method of
            Precise -> blockhashPrecise
            Quick -> blockhashQuick
   in f pixels width height bits

blockhashQuick :: Vector Word8 -> Int -> Int -> Int -> Hash
blockhashQuick pixels width height bits = Hash $ bitsToHexHash $ blocksToBits blocks (blockWidth * blockHeight)
  where
    blockWidth, blockHeight :: Int
    blockWidth = width `div` bits
    blockHeight = height `div` bits

    blocks :: Vector Int
    blocks = V.create $ do
      blocks <- MV.new (bits * bits)
      forM_ [0 .. bits - 1] (\y ->
        forM_ [0 .. bits - 1] (\x -> do
          valueRef <- newSTRef (0 :: Int)

          forM_ [0 .. blockHeight - 1] (\iy ->
            forM_ [0 .. blockWidth - 1] (\ix -> do
              let cx = x * blockWidth + ix
                  cy = y * blockHeight + iy
                  ii = (cy * width + cx) * 4

              let alpha = pixels ! (ii + 3)
              if (alpha == 0)
                 then modifySTRef valueRef (+765)
                 else modifySTRef valueRef (\x -> x + fromIntegral (pixels ! ii)
                                                    + fromIntegral (pixels ! (ii + 1))
                                                    + fromIntegral (pixels ! (ii + 2)))
              )
            )
          value <- readSTRef valueRef
          MV.write blocks (y * bits + x) value
          )
        )
      return blocks

blockhashPrecise :: Vector Word8 -> Int -> Int -> Int -> Hash
blockhashPrecise pixels width height bits =
  if (width `mod` bits == 0) && (height `mod` bits == 0)
     then blockhashQuick pixels width height bits
     else Hash $ bitsToHexHash $ blocksToBits blocks (floor (blockWidth * blockHeight)) -- FIXME: Is it okay to use floor?
  where
    blockWidth, blockHeight :: Float
    blockWidth = fromIntegral width / fromIntegral bits
    blockHeight = fromIntegral height / fromIntegral bits

    blocks :: Vector Float
    blocks = V.create $ do
      blocks <- MV.replicate (bits * bits) (0 :: Float)
      forM_ [0 .. height - 1] (\y -> do
        let yMod = fromIntegral (y + 1) `mod'` blockHeight
            (yInt, yFrac) = properFraction yMod
            weightTop = 1 - yFrac
            weightBottom = yFrac

        let blockTop = floor (fromIntegral y / blockHeight)
            blockBottom = if yInt > 0 || (y + 1) == height
                             then floor (fromIntegral y / blockHeight)
                             else ceiling (fromIntegral y / blockHeight)

        forM_ [0 .. width - 1] (\x -> do
          let xMod = fromIntegral (x + 1) `mod'` blockWidth
              (xInt, xFrac) = properFraction xMod
              weightLeft = 1 - xFrac
              weightRight = xFrac

          let blockLeft = floor (fromIntegral x / blockWidth)
              blockRight = if xInt > 0 || (x + 1) == width
                              then floor (fromIntegral x / blockWidth)
                              else ceiling (fromIntegral x / blockWidth)

          let ii = (y * width + x) * 4
              alpha = pixels ! (ii + 3)
              value = if alpha == 0
                         then 765
                         else fromIntegral (pixels ! ii) + fromIntegral (pixels ! (ii + 1)) + fromIntegral (pixels ! (ii + 2))

          MV.modify blocks (\x -> x + value * weightTop * weightLeft) (blockTop * bits + blockLeft)
          MV.modify blocks (\x -> x + value * weightTop * weightRight) (blockTop * bits + blockRight)
          MV.modify blocks (\x -> x + value * weightBottom * weightLeft) (blockBottom * bits + blockLeft)
          MV.modify blocks (\x -> x + value * weightBottom * weightRight) (blockBottom * bits + blockRight)
          )
        )
      return blocks

bitsToHexHash :: Vector Bool -> Vector Char
bitsToHexHash xs =
  let indices = V.enumFromThenTo 0 4 (V.length xs - 1)
   in V.map (\i -> intToDigit $ bitsToInt (V.slice i 4 xs)) indices
  where
    bitsToInt = V.foldl' (\acc x -> if x then acc * 2 + 1 else acc * 2) 0

blocksToBits :: (MV.Unbox a, Real a) => Vector a -> Int -> Vector Bool
blocksToBits blocks pixelsPerBlock = V.create $ do
  result <- MV.new (V.length blocks)
  forM_ [0..3] (\i -> do
    let m = median $ V.slice (i * bandSize) bandSize blocks
    forM_ [i * bandSize .. (i + 1) * bandSize - 1] (\j -> do
      let v = realToFrac (blocks ! j)
      -- Output a 1 if the block is brighter than the median.
      -- With images dominated by black or white, the median may
      -- end up being 0 or the max value, and thus having a lot
      -- of blocks of value equal to the median.  To avoid
      -- generating hashes of all zeros or ones, in that case output
      -- 0 if the median is in the lower value space, 1 otherwise
      MV.write result j (v > m || (abs (v - m) < 1 && m > halfBlockValue))
      )
    )
  return $ result
  where
    halfBlockValue :: Float
    halfBlockValue = fromIntegral $ pixelsPerBlock * 256 * 3 `div` 2

    bandSize :: Int
    bandSize = V.length blocks `div` 4

median :: (MV.Unbox a, Real a) => Vector a -> Float
median xs =
  let len = V.length xs
      ys = V.modify VA.sort xs
   in if even len
       -- FIXME: (mid + 1) should be (mid - 1),
        -- but leave it as it is to make it compatible with other implementations.
         then let mid = len `div` 2
               in realToFrac ((ys ! mid) + (ys ! (mid + 1))) / 2.0
         else let mid = floor (fromIntegral len / 2.0)
               in realToFrac (ys ! mid)
