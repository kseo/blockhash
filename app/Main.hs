module Main where

import qualified Codec.Picture as P
import Control.Monad (forM_)
import Data.Blockhash
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as V
import Options.Applicative

data Option = Option
  { quickFlag :: Bool
  , bits :: Int
  , filenames :: [FilePath]
  } deriving (Show)

opts = info (helper <*> cmdOpt)
  ( fullDesc
  <> progDesc "blockhash"
  )
  where
    cmdOpt = Option
       <$> switch
        ( short 'q'
        <> long "quick"
        <> help "Use quick hashing method")
       <*> option (auto :: ReadM Int)
        ( long "bits"
        <> short 'b'
        <> value 16
        <> help "Create hash of size N^2 bits." )
       <*> some (argument str (metavar "filenames"))

processImage :: FilePath -> Int -> Bool -> IO ()
processImage filename bits quick = do
  res <- P.readImage filename
  case res of
    Left err -> putStrLn ("Fail to read: " ++ filename)
    Right dynamicImage -> do
      let rgbaImage = P.convertRGBA8 dynamicImage
          pixels = VG.convert (P.imageData rgbaImage)
          image = Image { imagePixels = pixels
                        , imageWidth = P.imageWidth rgbaImage
                        , imageHeight = P.imageHeight rgbaImage }
          method = if quick then Quick else Precise
          hash = blockhash image bits method
      putStrLn (show hash ++ "  " ++ filename)

main :: IO ()
main = do
  option <- execParser opts
  forM_ (filenames option) (\filename ->
    processImage filename (bits option) (quickFlag option))

