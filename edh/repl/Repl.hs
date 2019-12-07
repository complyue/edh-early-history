{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Repl where

import           RIO                     hiding ( evaluate )

import qualified Data.Text                     as T

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

instance Show InterpretError where
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

repl :: EvalState -> [Text] -> InputT IO EvalState
repl state blkLines =
    getInputLine
            (case blkLines of
                [] -> "Đ: "
                _  -> "Đ| "
            )
        >>= \case
                Nothing -> case blkLines of
                    [] -> return state
                    _ -> -- TODO warn about premature EOF ?
                        return state
                Just text -> case blkLines of
                    [] -> case text of
                        "{" -> repl state [T.pack text]
                        _ ->
                            let code = T.pack text
                            in  case T.strip code of
                                    "" -> repl state []
                                    _  -> evalAndPrint code state

                    _ -> case text of
                        "}" ->
                            let
                                code =
                                    (T.unlines . reverse)
                                        $ (T.pack text)
                                        : blkLines
                            in  evalAndPrint code state
                        _ -> repl state $ (T.pack text) : blkLines
  where
    evalAndPrint :: Text -> EvalState -> InputT IO EvalState
    evalAndPrint c s = evaluate s c >>= \case
        Left err -> do
            outputStrLn $ show err
            repl state []
        Right (object, state') -> do
            outputStrLn $ show object
            repl state' []
