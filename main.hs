{-# LANGUAGE DeriveDataTypeable #-}

import           Control.DeepSeq
import           Data.Data
import           System.Console.CmdArgs.Implicit ((&=))
import qualified System.Console.CmdArgs.Implicit as Opt
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


collatz :: Integer -> IO Int
collatz n = go 0 n
  where
    go cnt n
      | n <= 1 = return cnt
      | even n = go (cnt+1) (n `div` 2)
      | odd  n = go (cnt+1) (3*n+1)

collatzP :: Integer -> Int
collatzP n = go 0 n
  where
    go cnt n
      | n <= 1 = cnt
      | even n = go (cnt+1) (n `div` 2)
      | odd  n = go (cnt+1) (3*n+1)


main :: IO ()
main = do
  Range lo up <- Opt.cmdArgs myOptParser
  (hiC, hiN) <-
    fmap maximum $
    parallelInterleaved $
    map (\(Range l u) -> do
            let ret = maximum $ map (\n -> (collatzP n, n)) $ [l..u]
            ret `deepseq` return ret) $
    splitMega $
    Range lo up
--  let (hiC, hiN) = maximum $ map (\n -> (collatzP n, n)) $ [lo..up]
  printf "%d %d\n" hiN hiC
  stopGlobalPool