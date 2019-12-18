{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Edh.Details.Tx
    ( EdhTxAddr
    , EdhTx
    , edhTxRead
    , edhTxWrite
    , runEdhTx
    )
where

import           Prelude

import           Control.Monad
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Concurrent
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


-- | A resolved attribute addressor
type EdhTxAddr = (Entity, AttrKey)

-- | The pending reads within a transaction
type EdhTxReads = MVar [(Entity, MVar [(AttrKey, MVar EdhValue)])]
-- | The pending writes within a transaction
type EdhTxWrites = MVar [(Entity, MVar [(AttrKey, MVar EdhValue)])]

-- todo seek better data structure to manage state of a tx
--      using list as is for pending ops is not optimal;
--      simply using strict 'Map Entity [(,)]' may be better,
--      but not trival as 'Entity' ('MVar' per se) lacks 'Ord' instance.
--      anyway, neither above is cache friendly.


-- | The transactional monad of Edh
newtype EdhTx a = EdhTx { unEdhTx :: ReaderT (EdhTxReads, EdhTxWrites) IO a }
    deriving (Functor, Applicative, Monad,
        MonadReader (EdhTxReads, EdhTxWrites),
        MonadIO, MonadFail)


runEdhTx :: EdhTx () -> IO ()
runEdhTx tx = do
    txReads  <- newMVar []
    txWrites <- newMVar []
    runReaderT (unEdhTx tx) (txReads, txWrites)
    driveEdhTx txReads txWrites

driveEdhTx :: EdhTxReads -> EdhTxWrites -> IO ()
driveEdhTx txReads txWrites = do
    rs <- takeMVar txReads
    ws <- takeMVar txWrites
    if Prelude.null rs && Prelude.null ws
        then return ()
        else do
            -- do atomic reads&writes per entity

            -- loop another iteration
            putMVar txReads  []
            putMVar txWrites []
            driveEdhTx txReads txWrites


edhTxRead :: EdhTxAddr -> (EdhValue -> EdhTx ()) -> EdhTx ()
edhTxRead addr r = ask >>= liftIO . schdRead
  where
    schdRead :: (EdhTxReads, EdhTxWrites) -> IO ()
    schdRead tx@(txReads, _txWrites) = do
        p <- newEmptyMVar
        modifyMVar_ txReads $ edhTxEnqOp addr p
        void $ forkIO $ do
            v <- readMVar p
            runReaderT (unEdhTx $ r v) tx

edhTxWrite :: EdhTxAddr -> (MVar EdhValue -> EdhTx ()) -> EdhTx ()
edhTxWrite addr w = ask >>= liftIO . schdWrite
  where
    schdWrite :: (EdhTxReads, EdhTxWrites) -> IO ()
    schdWrite tx@(_txReads, txWrites) = do
        p <- newEmptyMVar
        modifyMVar_ txWrites $ edhTxEnqOp addr p
        void $ forkIO $ runReaderT (unEdhTx $ w p) tx


edhTxEnqOp
    :: EdhTxAddr
    -> MVar EdhValue
    -> [(Entity, MVar [(AttrKey, MVar EdhValue)])]
    -> IO [(Entity, MVar [(AttrKey, MVar EdhValue)])]
edhTxEnqOp (ent, key) p rs = edhTxEnqOp' rs
  where
    edhTxEnqOp'
        :: [(Entity, MVar [(AttrKey, MVar EdhValue)])]
        -> IO [(Entity, MVar [(AttrKey, MVar EdhValue)])]
    edhTxEnqOp' [] = do
        ps <- newMVar [(key, p)]
        return $ (ent, ps) : rs
    edhTxEnqOp' ((ent', ops) : rest) = if ent' /= ent
        then edhTxEnqOp' rest
        else do
            modifyMVar_ ops $ \ps -> return $ (key, p) : ps
            return rs



