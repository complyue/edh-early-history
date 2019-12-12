{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Repl where

import           Prelude
import           Debug.Trace

import           Data.Text                     as T

import           System.Console.Haskeline

import           Text.Megaparsec

import           Language.Edh.Control
import           Language.Edh.Runtime
import           Language.Edh.Interpreter
import           Language.Edh.Batteries


doRead :: [Text] -> InputT IO (Maybe Text)
doRead pendingLines =
    handleInterrupt (return $ Just "")
        $   withInterrupt
        $   getInputLine case pendingLines of
                [] -> "Đ: "
                _  -> "Đ| " <> show (Prelude.length pendingLines) <> " : "
        >>= \case
                Nothing -> case pendingLines of
                    [] -> return Nothing
                    _ -> -- TODO warn about premature EOF ?
                        return Nothing
                Just text ->
                    let code = T.pack text
                    in
                        case pendingLines of
                            [] -> case T.stripEnd code of
                                "{" -> -- an unindented `{` marks start of multi-line input
                                    doRead [""]
                                _ -> case T.strip code of
                                    "" -> -- got an empty line in single-line input mode
                                        doRead [] -- start over in single-line input mode
                                    _ -> -- got a single line input
                                        return $ Just code
                            _ -> case T.stripEnd code of
                                "}" -> -- an unindented `}` marks end of multi-line input
                                    return
                                        $ Just
                                        $ (T.unlines . Prelude.reverse)
                                        $ Prelude.init pendingLines
                                _ -> -- got a line in multi-line input mode
                                    doRead $ code : pendingLines


doEval
    :: EdhWorld -> Module -> Text -> InputT IO (Either InterpretError EdhValue)
doEval world modu code = evalEdhSource world modu code


doPrint :: (Either InterpretError EdhValue) -> InputT IO ()
doPrint = \case
    Left err -> case err of
        EdhParseError _ -> do
            outputStrLn "* 😓 *"
            outputStrLn $ show err
        EdhEvalError _ -> do
            outputStrLn "* 😱 *"
            outputStrLn $ show err
    Right o -> case o of
        EdhNil -> return ()
        _      -> outputStrLn $ show o


doLoop :: EdhWorld -> Module -> InputT IO ()
doLoop world modu = (doRead []) >>= \case
    Nothing   -> return () -- reached EOF (end-of-feed)
    Just code -> if code == ""
        then doLoop world modu  -- ignore empty code
        else do -- got one piece of code
            r <- doEval world modu code
            doPrint r
            doLoop world modu

