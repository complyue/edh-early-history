{-# LANGUAGE LambdaCase #-}

module Main where

import           Prelude
import           Debug.Trace

import           Language.Edh.Control
import           Language.Edh.Runtime
import           Language.Edh.Interpreter
import           Language.Edh.Batteries

import           Repl                           ( doLoop )

main :: IO ()
main = do
    putStrLn "(EDHi)"

    world <- createEdhWorldWithBatteries

    trace "got world" $ return ()

    runEdhModule world "<interactive>" "pass" >>= \case
        Left  err  -> error $ show err
        Right modu -> doLoop world modu

