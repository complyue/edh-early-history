
module Language.Edh.Interpreter.Evaluate where

import           Prelude

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict

import           Data.IORef
import           Foreign.C.String
import           System.IO.Unsafe
import           Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

import           Data.Lossless.Decimal         as D

import           Language.Edh.AST
import           Language.Edh.Parser
import           Language.Edh.Parser.Details
import           Language.Edh.Runtime


evalProgram :: MonadIO m => Module -> SeqStmts -> m ()
evalProgram modu stmts = liftIO $ do
    return ()

