{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

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
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Concurrent

import qualified Data.Map.Strict               as Map

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
  -- TODO eliminate or fine tune the magical `retryCnt` value here
  driveEdhTx 3 txReads txWrites

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


-- TODO need theoretical proof & practical verification that this
--      will always converge to proper halt; if `retryCnt` is
--      unavoidable, the optimal safe value it can range in.

driveEdhTx :: Int -> EdhTxReads -> EdhTxWrites -> IO ()
driveEdhTx retryCnt txReads txWrites = do
  txrs <- takeMVar txReads
  txws <- takeMVar txWrites
  if null txrs && null txws
    then
      -- hope this leads to proper halt
      when (retryCnt > 0) $ yield >> driveEdhTx (retryCnt - 1) txReads txWrites
    else do
      -- kickoff atomic reads&writes per entity
      scanEntities txrs txws

      -- allow current running tx ops to register further ops
      putMVar txReads  []
      putMVar txWrites []

      -- loop another iteration
      driveEdhTx retryCnt txReads txWrites

 where

  scanEntities
    :: [(Entity, MVar [(AttrKey, MVar EdhValue)])]
    -> [(Entity, MVar [(AttrKey, MVar EdhValue)])]
    -> IO ()
  scanEntities []         [] = return ()
  scanEntities (r : txrs) [] = do
    void $ forkIO $ perEntity (Just r) Nothing
    scanEntities txrs []
  scanEntities [] (w : txws) = do
    void $ forkIO $ perEntity Nothing (Just w)
    scanEntities [] txws
  scanEntities (r@(ent, _) : txrs) txws@(_ : _) =
    let (w, txws') = takeOutFromList ((== ent) . fst) txws
    in  do
          void $ forkIO $ perEntity (Just r) w
          scanEntities txrs txws'

  perEntity
    :: Maybe (Entity, MVar [(AttrKey, MVar EdhValue)])
    -> Maybe (Entity, MVar [(AttrKey, MVar EdhValue)])
    -> IO ()
  perEntity ers ews = case ers of
    Nothing -> case ews of
      Nothing        -> error "bug"
      Just (ent, ws) -> readMVar ws >>= \ws' -> modifyMVar_ ent $ \e ->
        flip Map.union e . Map.fromList <$> forM
          ws'
          (\(key, vv) -> (key, ) <$> readMVar vv)
    Just (ent, rs) -> case ews of
      Nothing -> readMVar rs >>= \rs' -> readMVar ent >>= \e ->
        forM_ rs' $ \(key, vv) -> case Map.lookup key e of
          Nothing -> error "bug"
          Just v  -> putMVar vv v
      Just (_ent', ws) -> do
        rs' <- readMVar rs
        ws' <- readMVar ws
        modifyMVar_ ent $ \e -> do
          forM_ rs' $ \(key, vv) -> case Map.lookup key e of
            Nothing -> error "bug"
            Just v  -> putMVar vv v
          flip Map.union e . Map.fromList <$> forM
            ws'
            (\(key, vv) -> (key, ) <$> readMVar vv)

