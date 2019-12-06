{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Repl where

import           RIO                     hiding ( evaluate )

import qualified Data.Text                     as T

import qualified GHC.Show                      as G

import           System.Console.Haskeline

import           Language.Edh.Compiler.Lexer    ( lex )
import           Language.Edh.Compiler.Parser   ( parse )
import           Language.Edh.Runtime.Evaluator ( evalWithState )
import           Language.Edh.Runtime.Evaluator.Types
                                                ( EvalError
                                                , EvalState
                                                )
import           Language.Edh.Runtime.Evaluator.Object
                                                ( Object )
import           Language.Edh.Compiler.ParserT  ( ParserError )


data InterpretError = P ParserError | E EvalError

instance G.Show InterpretError where
    show (P p) = show p
    show (E p) = show p

evaluate
    :: EvalState
    -> Text
    -> InputT IO (Either InterpretError (Object, EvalState))
evaluate state input = do
    let result = lex input >>= parse
    case result of
        Right ast -> liftIO $ evalWithState ast state >>= \case
            Right x   -> return $ Right x
            Left  err -> return $ Left (E err)
        Left err -> return $ Left (P err)

repl :: EvalState -> InputT IO EvalState
repl state = getInputLine "Đ: " >>= \case
    Nothing -> return state
    Just text ->
        let code = T.pack text
        in  case T.strip code of
                "" -> repl state
                _  -> (evaluate state code) >>= \case
                    Left err -> do
                        outputStrLn $ show err
                        repl state
                    Right (object, state') -> do
                        outputStrLn $ show object
                        repl state'
