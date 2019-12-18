{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Language.Edh.Details.Tx
  ( EdhTxAddr
  , EdhTx
  , edhTxRead
  , edhTxWrite
  , throwEdh
  , runEdhTx
  )
where

import           Prelude

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Concurrent


import qualified Data.Map.Strict               as Map

import           Language.Edh.Details.RtTypes
import           Language.Edh.Details.Utils


throwEdh :: Exception e => e -> EdhTx a
throwEdh e = liftIO $ throwIO e


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


runEdhTx :: MVar () -> EdhTx () -> IO ()
runEdhTx halt tx = do
  txReads  <- newMVar []
  txWrites <- newMVar []
  runReaderT (unEdhTx tx) (txReads, txWrites)
  driveEdhTx halt txReads txWrites

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


driveEdhTx :: MVar () -> EdhTxReads -> EdhTxWrites -> IO ()
driveEdhTx halt txReads txWrites = isEmptyMVar halt >>= \case
  False -> return ()
  True  -> do
    txrs <- takeMVar txReads
    txws <- takeMVar txWrites
    -- kickoff atomic reads&writes per entity
    scanEntities txrs txws
    -- allow current running tx ops to register further ops
    putMVar txReads  []
    putMVar txWrites []
    -- loop another iteration
    yield
    driveEdhTx halt txReads txWrites

 where

  scanEntities
    :: [(Entity, MVar [(AttrKey, MVar EdhValue)])]
    -> [(Entity, MVar [(AttrKey, MVar EdhValue)])]
    -> IO ()
  scanEntities []                  [] = return ()
  scanEntities ((ent, ers) : txrs) [] = do
    ers' <- readMVar ers
    void $ forkIO $ perEntity ent ers' []
    scanEntities txrs []
  scanEntities [] ((ent, ews) : txws) = do
    ews' <- readMVar ews
    void $ forkIO $ perEntity ent [] ews'
    scanEntities [] txws
  scanEntities ((ent, ers) : txrs) txws@(_ : _) = do
    ers' <- readMVar ers
    case w of
      Nothing          -> void $ forkIO $ perEntity ent ers' []
      Just (_ent, ews) -> do
        ews' <- readMVar ews
        void $ forkIO $ perEntity ent ers' ews'
    scanEntities txrs txws'
    where (w, txws') = takeOutFromList ((== ent) . fst) txws

  perEntity
    :: Entity
    -> [(AttrKey, MVar EdhValue)]
    -> [(AttrKey, MVar EdhValue)]
    -> IO ()
  perEntity ent ers ews = modifyMVar_ ent $ \e -> do
    forM_ ers $ \(key, vv) -> case Map.lookup key e of
      Nothing -> error "bug"
      Just v  -> putMVar vv v
    flip Map.union e . Map.fromList <$> forM
      ews
      (\(key, vv) -> (key, ) <$> readMVar vv)

