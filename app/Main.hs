module Main where

import Data.Blockhash
import Options.Applicative

data Option = Option
  { quickFlag :: Bool
  , bits :: Int
  , debugFlag :: Bool
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
       <*> switch
        ( long "debug"
        <> help "Print hashes as 2D maps (for debugging)" )
       <*> some (argument str (metavar "filenames"))

main :: IO ()
main = do
  option <- execParser opts
  print option

