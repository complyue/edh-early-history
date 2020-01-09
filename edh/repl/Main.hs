
module Main where

import           Prelude
-- import           Debug.Trace

import           System.Console.Haskeline

import           Language.Edh.Runtime
import           Language.Edh.Interpreter
import           Language.Edh.Batteries

import           Repl                           ( doLoop )


inputSettings :: Settings IO
inputSettings = Settings { complete       = \(_left, _right) -> return ("", [])
                         , historyFile    = Nothing
                         , autoAddHistory = True
                         }


main :: IO ()
main = do

  -- todo create a logger coop'ing with haskeline specifically ?
  logger <- defaultEdhLogger

  runInputT inputSettings $ do

    outputStrLn "(EDHi)"

    world <- createEdhWorld logger
    installEdhBatteries world

    modu <- createEdhModule world "<interactive>"
    doLoop world modu

