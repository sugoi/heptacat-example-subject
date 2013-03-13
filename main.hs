{-# LANGUAGE DeriveDataTypeable #-}

import           Data.Data
import           System.Console.CmdArgs.Implicit ((&=))
import qualified System.Console.CmdArgs.Implicit as Opt
import           Text.Printf

data Range
  = Range { lower :: Integer, upper :: Integer}
    deriving (Show, Typeable, Data)

myOptParser :: Range
myOptParser = Range
  { lower = Opt.def &= Opt.argPos 0 , upper = Opt.def &= Opt.argPos 1}


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
  let (hiC, hiN) = maximum $ map (\n -> (collatz n, n)) [lo..up]
  printf "%d %d\n" hiN hiC
