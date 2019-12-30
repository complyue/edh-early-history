
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
main = runInputT inputSettings $ do

  outputStrLn "(EDHi)"

  world <- createEdhWorld
  installEdhBatteries world

  runEdhModule world "<interactive>" "pass" >>= \case
    Left  err  -> error $ show err
    Right modu -> doLoop world modu

