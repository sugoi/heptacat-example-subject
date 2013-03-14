{-# LANGUAGE DeriveDataTypeable #-}

import           Control.DeepSeq
import           Data.Data
import qualified Data.Progress.Meter as PM
import qualified Data.Progress.Tracker as PM
import qualified Data.Quantity as PM
import           System.Console.CmdArgs.Implicit ((&=))
import qualified System.Console.CmdArgs.Implicit as Opt
import           System.Directory
import           System.IO
import           Text.Printf
import           Control.Concurrent.ParallelIO.Global (parallelInterleaved, stopGlobalPool)

data Range
  = Range { lower :: Integer, upper :: Integer}
    deriving (Show, Typeable, Data)

splitMega :: Range -> [Range]
splitMega x@(Range l u)
  | u <= l + mega = [x]
  | otherwise     = Range l (l+mega-1) : splitMega (Range (l+mega) u)
  where
    mega = 1000000


myOptParser :: Range
myOptParser = Range
  { lower = Opt.def &= Opt.argPos 0, upper = Opt.def &= Opt.argPos 1}


collatz :: Integer -> Int
collatz n = go 0 n
  where
    go cnt n
      | n <= 1 = cnt
      | even n = go (cnt+1) (n `div` 2)
      | odd  n = go (cnt+1) (3*n+1)


main :: IO ()
main = do
  Range lo up <- Opt.cmdArgs myOptParser
  let tasks = splitMega $ Range lo up

  prog <- PM.newProgress "task" (fromIntegral $ 1000000 * length tasks)
  pm   <- PM.newMeter prog "" 80 (PM.renderNums PM.siOpts 1)
  PM.addComponent pm prog
  mtid <- PM.autoDisplayMeter pm 1 (PM.displayMeter stderr)

  (hiC, hiN) <-
    fmap maximum $
    parallelInterleaved $
    flip map tasks $ \(Range l u) -> do
      let ret = maximum $ map (\n -> (collatz n, n)) $ [l..u]
      ret `deepseq` PM.incrP prog 1000000
      return ret

  createDirectoryIfMissing True "output"
  withFile "output/ans.txt" WriteMode $ \fp -> do
    hPrintf fp "%d %d\n" hiN hiC

  PM.killAutoDisplayMeter pm mtid
  hPutStrLn stderr ""
  stopGlobalPool