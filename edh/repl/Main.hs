{-# LANGUAGE LambdaCase #-}

module Main where

import           Prelude

import           Language.Edh.Control
import           Language.Edh.Runtime
import           Language.Edh.Interpreter
import           Language.Edh.Batteries

import           Repl                           ( doLoop )

main :: IO ()
main = do
    putStrLn "(EDHi)"

    world <- createEdhWorldWithBatteries

    runEdhModule world "<interactive>" "pass" >>= \case
        Left  err  -> error $ show err
        Right modu -> doLoop world modu

