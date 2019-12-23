
module Language.Edh.Batteries.Utils where

import           Prelude
import           Debug.Trace

import           Control.Monad.Reader
import           Control.Concurrent.STM
import           Control.Monad.IO.Class

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Data.IORef
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           System.Mem.Weak
import           System.IO.Unsafe

import           Data.Lossless.Decimal         as D

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime
import           Language.Edh.Event


consProc :: EdhProcedure
consProc (PackSender [SendPosArg !lhExpr, SendPosArg !rhExpr]) _ !exit = do
  !pgs <- ask
  let !callerCtx = edh'context pgs
      !scope     = contextScope callerCtx
  -- make sure left hand and right hand values are evaluated in same tx
  local (\s -> s { edh'in'tx = True }) $ evalExpr lhExpr $ \(_, lhVal) ->
    evalExpr rhExpr
      $ \(_, rhVal) -> exitEdhProc exit (scope, EdhPair lhVal rhVal)
consProc !argsSender _ _ =
  throwEdh $ EvalError $ "Unexpected operator args: " <> T.pack
    (show argsSender)


packProc :: EdhProcedure
packProc !argsSender _ !exit = packEdhArgs argsSender exit


concatProc :: EdhProcedure
concatProc argsSender procScope exit = undefined


typeProc :: EdhProcedure
typeProc argsSender procScope exit = undefined
  -- packEdhArgs  argsSender $ \(ArgsPack !args !kwargs) ->

  --   let !argsType = map edhTypeOf args
  --       kwargsType = -- note: leave this lazy, not always needed
  --           Map.fromList $ (<$> Map.toAscList kwargs) $ \(attrName, val) ->
  --             (ItemByStr attrName, edhTypeOf val)
  --   in  if null kwargs
  --         then case argsType of
  --           [t] -> exit (procScope, t)
  --           _   -> exit (procScope, EdhTuple argsType)
  --         else do
  --           d <- liftIO $ newTVarIO $ Map.union kwargsType $ Map.fromAscList
  --             [ (ItemByNum (fromIntegral i), t)
  --             | (i, t) <- zip [(0 :: Int) ..] argsType
  --             ]
  --           exit (procScope, EdhDict (Dict d))


dictProc :: EdhProcedure
dictProc argsSender procScope exit = undefined
  -- packEdhArgs  argsSender $ \(ArgsPack !args !kwargs) ->
  --   let kwDict =
  --           Map.fromAscList $ (<$> Map.toAscList kwargs) $ \(attrName, val) ->
  --             (ItemByStr attrName, val)
  --   in  do
  --         d <- liftIO $ newTVarIO $ Map.union kwDict $ Map.fromAscList
  --           [ (ItemByNum (fromIntegral i), t)
  --           | (i, t) <- zip [(0 :: Int) ..] args
  --           ]
  --         exit (procScope, EdhDict (Dict d))

