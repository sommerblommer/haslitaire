module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T
import System.Random 
someFunc :: IO ()
someFunc = putStrLn "someFunc"
