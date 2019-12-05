{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Repl where

import           RIO                     hiding ( evaluate )

import           Data.Text.IO                   ( putStrLn )
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

read :: IO (Maybe Text)
read = runInputT defaultSettings $ (fmap . fmap) T.pack $ getInputLine "Đ: "

data InterpretError = P ParserError | E EvalError

instance G.Show InterpretError where
    show (P p) = show p
    show (E p) = show p

evaluate :: EvalState -> Text -> IO (Either InterpretError (Object, EvalState))
evaluate state input = do
    let result = lex input >>= parse
    case result of
        Right ast -> evalWithState ast state >>= \case
            Right x   -> return $ Right x
            Left  err -> return $ Left (E err)
        Left err -> return $ Left (P err)

repl :: EvalState -> IO EvalState
repl state = read >>= \case
    Nothing   -> return state
    Just text -> case T.strip text of
        "" -> repl state
        _  -> evaluate state text >>= \case
            Left err -> do
                putStrLn $ tshow err
                repl state
            Right (object, state') -> do
                putStrLn $ tshow object
                repl state'
