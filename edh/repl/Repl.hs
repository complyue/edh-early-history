{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Repl where

import           Prelude
import           Debug.Trace

import           Data.Text                     as T

import           System.Console.Haskeline

import           Language.Edh.Control
import           Language.Edh.Runtime
import           Language.Edh.Interpreter
import           Language.Edh.Batteries


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
                                        $ (T.unlines . Prelude.reverse)
                                        $ code
                                        : pendingLines
                                _ -> -- got a line in multi-line input mode
                                    doRead $ code : pendingLines


doEval :: EdhWorld -> Module -> Text -> IO (Either InterpretError EdhValue)
doEval world modu code = evalEdhSource world modu code


doPrint :: (Either InterpretError EdhValue) -> InputT IO ()
doPrint = \case
    Left err -> case err of
        EdhParseError err -> do
            outputStrLn "* 😓 *"
            outputStrLn $ show err
        EdhEvalError err -> do
            outputStrLn "* 😱 *"
            outputStrLn $ show err
    Right o -> case o of
        EdhNil -> return ()
        _      -> outputStrLn $ show o


doLoop :: EdhWorld -> Module -> IO ()
doLoop world modu = (runInputT inputSettings $ doRead []) >>= \case
    Nothing   -> return () -- reached EOF (end-of-feed)
    Just code -> if code == ""
        then doLoop world modu  -- ignore empty code
        else do -- got one piece of code
            r <- doEval world modu code
            runInputT inputSettings $ doPrint r
            doLoop world modu

