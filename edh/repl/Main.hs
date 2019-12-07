module Main where

import           RIO

import           System.IO
import           Language.Edh.Runtime.Evaluator.Types
                                                ( createEmptyState )

import           Repl                           ( doLoop )

main :: IO ()
main = do
    putStrLn "(EDHi)"

    createEmptyState >>= doLoop
