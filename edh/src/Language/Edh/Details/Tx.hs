
module Language.Edh.Details.Tx where

import           Prelude

import           Control.Exception
import           Control.Monad.Except
import           Control.Concurrent.MVar

import           Data.IORef
import           Foreign.C.String
import           System.IO.Unsafe
import           Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Details.RtTypes
import           Language.Edh.Details.Utils


-- | A transactional read
type EdhTxRead = (Entity, AttrKey)
-- | A transactional write
type EdhTxWrite = (Entity, AttrKey, MVar EdhValue)
-- | The pending reads within a transaction
type EdhTxReads = [(Entity, [(AttrKey, MVar EdhValue)])]
-- || The pending writes within a transaction
type EdhTxWrites = [(Entity, [(AttrKey, MVar EdhValue)])]
-- | An Edh transaction
type EdhTx = (EdhTxReads, EdhTxWrites)







-- | A transaction for grouped assignments of attributes onto entities
--
-- TODO a list here should not scale well to large transactions, but
--      can't use Entity (IORef per se) as Map key, need more tweaks.
type EdhAssignTx = [(Entity, IORef [(AttrKey, EdhValue)])]

prepareEdhAssign
    :: Context -> EdhAssignTx -> AttrAddressor -> EdhValue -> IO EdhAssignTx
prepareEdhAssign ctx tx addr v = do
    k <- resolveAddr addr
    sched2Tx (k, v) tx
  where
    ent   = objEntity $ thisObject scope
    scope = contextScope ctx

    sched2Tx :: (AttrKey, EdhValue) -> EdhAssignTx -> IO EdhAssignTx
    sched2Tx u [] = do
        lr <- newIORef [u]
        return $ (ent, lr) : tx
    sched2Tx u ((e, lr) : rest) = if e /= ent
        then sched2Tx u rest
        else do
            modifyIORef' lr (\l -> u : l)
            return tx

    resolveAddr :: AttrAddressor -> IO AttrKey
    resolveAddr (NamedAttr attrName) = return $ AttrByName attrName
    -- resolveAddr (SymbolicAttr symName) =
        -- resolveEdhObjAttr scope symName >>= \case
        --     Just (EdhSymbol symVal) -> return $ AttrBySym symVal
        --     Nothing ->
        --         throwIO
        --             $  EvalError
        --             $  "No symbol named "
        --             <> T.pack (show symName)
        --             <> " available"
        --     Just v ->
        --         throwIO
        --             $  EvalError
        --             $  "Expect a symbol named "
        --             <> T.pack (show symName)
        --             <> " but got: "
        --             <> T.pack (show v)


