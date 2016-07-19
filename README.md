blockhash [![Build Status](https://travis-ci.org/kseo/blockhash.svg?branch=master)](https://travis-ci.org/kseo/blockhash)
=========

This is a perceptual image hash calculation tool based on algorithm described in
Block Mean Value Based Image Perceptual Hashing by Bian Yang, Fan Gu and Xiamu Niu.
Visit [the website][blockhash] for further information.

[blockhash]: http://blockhash.io/

## Program

```
Usage: blockhash [-q|--quick] [-b|--bits ARG] filenames
  blockhash

Available options:
  -h,--help                Show this help text
  -q,--quick               Use quick hashing method
  -b,--bits ARG            Create hash of size N^2 bits.
```

## Library

The example code below uses [JuicyPixels][JuicyPixels] to load images and prints
the hash to stdout.

```haskell
import qualified Codec.Picture as P
import Data.Blockhash
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as V

printHash :: FilePath -> IO ()
printHash :: filename = do
  res <- P.readImage filename
  case res of
    Left err -> putStrLn ("Fail to read: " ++ filename)
    Right dynamicImage -> do
      let rgbaImage = P.convertRGBA8 dynamicImage
          pixels = VG.convert (P.imageData rgbaImage)
          image = Image { imagePixels = pixels
                        , imageWidth = P.imageWidth rgbaImage
                        , imageHeight = P.imageHeight rgbaImage }
          hash = blockhash image 16 Precise
      putStrLn (show hash)
```

[JuicyPixels]: https://hackage.haskell.org/package/JuicyPixels-3.2.7.2
