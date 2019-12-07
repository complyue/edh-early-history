module Main where

import           RIO

import           System.Console.Haskeline

import           Language.Edh.Runtime.Evaluator.Types
                                                ( createEmptyState )

import           Repl                           ( repl )

main :: IO ()
main =
    runInputT
            (Settings { complete       = \(_left, _right) -> return ("", [])
                      , historyFile    = Nothing
                      , autoAddHistory = True
                      }
            )
        $ do
              outputStrLn "(EDHi)"
              state   <- liftIO createEmptyState
              _state' <- repl state []
              return ()
