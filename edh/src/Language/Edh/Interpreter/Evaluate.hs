
module Language.Edh.Interpreter.Evaluate where

import           Prelude

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict

import           Data.Typeable
import           Data.IORef
import           Foreign.C.String
import           System.IO.Unsafe
import           Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

import           Data.Lossless.Decimal         as D

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime
import           Language.Edh.Parser


runEdhProgram
    :: MonadIO m => Module -> SeqStmts -> m (Either EvalError EdhValue)
runEdhProgram modu stmts = liftIO $ do

    void $ mapM_ (Prelude.putStrLn . show) stmts

    if Prelude.null stmts
        then return $ Right nil
        else return $ Right $ EdhString $ T.pack $ show $ Prelude.tail stmts
    -- return $ Left $ EvalError "not impl."

