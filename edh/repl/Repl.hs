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
                                                ( Object(ONil) )
import           Language.Edh.Compiler.ParserT  ( ParserError )


data InterpretError = P ParserError | E EvalError

instance Show InterpretError where
    show (P p) = show p
    show (E p) = show p

inputSettings :: Settings IO
inputSettings = Settings { complete       = \(_left, _right) -> return ("", [])
                         , historyFile    = Nothing
                         , autoAddHistory = True
                         }

doRead :: [Text] -> InputT IO (Maybe Text)
doRead pendingLines =
    handleInterrupt (return $ Just "")
        $   withInterrupt
        $   getInputLine case pendingLines of
                [] -> "Đ: "
                _  -> "Đ| "
        >>= \case
                Nothing -> case pendingLines of
                    [] -> return Nothing
                    _ -> -- TODO warn about premature EOF ?
                        return Nothing
                Just text ->
                    let code = T.pack text
                    in  case pendingLines of
                            [] -> case T.stripEnd code of
                                "{" -> -- an unindented `{` marks start of multi-line input
                                    doRead [code]
                                _ -> case T.strip code of
                                    "" -> -- got an empty line in single-line input mode
                                        doRead [] -- start over in single-line input mode
                                    _ -> -- got a single line input
                                        return $ Just code
                            _ -> case T.stripEnd code of
                                "}" -> -- an unindented `}` marks end of multi-line input
                                    return
                                        $ Just
                                        $ (T.unlines . reverse)
                                        $ code
                                        : pendingLines
                                _ -> -- got a line in multi-line input mode
                                    doRead $ code : pendingLines


doEval :: EvalState -> Text -> IO (EvalState, Either InterpretError Object)
doEval s c = case (lex c >>= parse) of
    Right ast -> evalWithState ast s >>= \case
        Right (o, s') -> return (s', Right o)
        Left  err     -> return (s, Left (E err))
    Left err -> return (s, Left (P err))


doPrint :: (Either InterpretError Object) -> InputT IO ()
doPrint = \case
    Left err -> do
        outputStrLn "* 😱 *"
        outputStrLn $ show err
    Right o -> case o of
        ONil -> return ()
        _    -> do
            outputStrLn $ show o


doLoop :: EvalState -> IO ()
doLoop s = (runInputT inputSettings $ doRead []) >>= \case
    Nothing -> return () -- reached EOF (end-of-feed)
    Just c  -> if c == ""
        then doLoop s -- ignore empty code
        else do -- got one piece of code
            (s', r) <- doEval s c
            runInputT inputSettings $ doPrint r
            doLoop s'

