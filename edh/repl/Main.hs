module Main where

import           RIO
import           Data.Text.IO                   ( putStrLn )

import           Evaluator.Types                ( createEmptyState )
import           Repl                           ( repl )

main :: IO ()
main = putStrLn ("(EDHi)" :: Text) >> void (createEmptyState >>= repl)

