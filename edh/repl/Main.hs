module Main where

import           RIO
import           Data.Text.IO                   ( putStrLn )
import           Repl                           ( repl )

main :: IO ()
main = putStrLn ("The Monkey programming language REPL" :: Text) >> repl
