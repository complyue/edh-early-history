{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Edh.Details.Tx
  ( EdhTxAddr
  , EdhProg(..)
  , runEdhProg
  , runEdhTx
  , EdhAttrRead
  , EdhAttrWrite
  , throwEdh
  , EdhTxState(..)
  , EdhTxOps(..)
  , EdhOpsPack
  , asyncEdh
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


throwEdh :: Exception e => e -> EdhProg a
throwEdh e = liftIO $ throwIO e

asyncEdh :: ThreadId -> IO () -> IO ()
asyncEdh masterThread f = void $ forkIO $ void $ handle onExc f
 where
  onExc :: SomeException -> IO ()
  onExc e = throwTo masterThread e >> throwIO ThreadKilled


-- | A resolved attribute addressor
type EdhTxAddr = (Entity, AttrKey)


-- | Transactional operations packed per entity basis
type EdhOpsPack = [(Entity, [(AttrKey, MVar EdhValue)])]
-- todo seek better data structure to manage state of a tx.
--      using list as is for pending ops is not optimal;
--      simply using strict 'Map Entity [(,)]' may be better,
--      but not trival as 'Entity' ('MVar' per se) lacks 'Ord' instance.
--      anyway, neither above is cache friendly.
_packTxOp :: EdhTxAddr -> MVar EdhValue -> EdhOpsPack -> EdhOpsPack
_packTxOp (ent, key) var = packTxOp' []
 where
  packTxOp' :: EdhOpsPack -> EdhOpsPack -> EdhOpsPack
  packTxOp' prefix []                     = (ent, [(key, var)]) : prefix
  packTxOp' prefix (r@(ent', eos) : rest) = if ent' /= ent
    then packTxOp' (r : prefix) rest
    else (ent, (key, var) : eos) : prefix ++ rest


-- | All operations per a transaction
data EdhTxOps = EdhTxOps {
    edh'tx'reads :: !EdhOpsPack
    , edh'tx'writes :: !EdhOpsPack
  }

data EdhTxState = EdhTxState {
    edh'tx'master :: !ThreadId
    , edh'tx'ops :: !(MVar EdhTxOps)
  }

-- | The transactional monad of Edh
newtype EdhProg a = EdhProg { unEdhProg :: ReaderT EdhTxState IO a }
    deriving (Functor, Applicative, Monad,
        MonadReader EdhTxState,
        MonadIO, MonadFail)


runEdhProg :: MVar () -> EdhProg () -> IO ()
runEdhProg halt prog = do
  masterThread <- myThreadId
  txOps        <- newEmptyMVar
  runReaderT (unEdhProg prog) (EdhTxState masterThread txOps)
  driveEdhTx masterThread halt txOps


type EdhAttrRead = (EdhTxAddr, EdhValue -> EdhProg ())
type EdhAttrWrite = (EdhTxAddr, MVar EdhValue -> EdhProg ())

runEdhTx :: [EdhAttrRead] -> [EdhAttrWrite] -> EdhProg ()
runEdhTx attrReads attrWrites = do
  txs@(EdhTxState masterThread txOps) <- ask
  let schedTx :: IO ()
      schedTx = do
        readPack  <- foldM packRead [] attrReads
        writePack <- foldM packWrite [] attrWrites
        putMVar txOps $ EdhTxOps readPack writePack

      packRead :: EdhOpsPack -> EdhAttrRead -> IO EdhOpsPack
      packRead pck (addr, rdr) = do
        var <- newEmptyMVar
        let pck' = _packTxOp addr var pck
        asyncEdh masterThread $ do
          v <- readMVar var
          runReaderT (unEdhProg $ rdr v) txs
        return pck'

      packWrite :: EdhOpsPack -> EdhAttrWrite -> IO EdhOpsPack
      packWrite pck (addr, wtr) = do
        var <- newEmptyMVar
        let pck' = _packTxOp addr var pck
        asyncEdh masterThread $ runReaderT (unEdhProg $ wtr var) txs
        return pck'

  liftIO schedTx


driveEdhTx :: ThreadId -> MVar () -> MVar EdhTxOps -> IO ()
driveEdhTx masterThread halt txOps = do
  -- block wait next tx to come
  EdhTxOps txrs txws <- readMVar txOps
  -- kickoff atomic reads&writes per entity
  launchTx txrs txws
  yield
  -- check halt 
  isEmptyMVar halt >>= \case
    -- shall halt
    False -> return ()
    -- loop another iteration
    True  -> driveEdhTx masterThread halt txOps

 where

  launchTx
    :: [(Entity, [(AttrKey, MVar EdhValue)])]
    -> [(Entity, [(AttrKey, MVar EdhValue)])]
    -> IO ()
  launchTx []                  [] = return ()
  launchTx ((ent, ers) : txrs) [] = do
    asyncEdh masterThread $ perEntity ent ers []
    launchTx txrs []
  launchTx [] ((ent, ews) : txws) = do
    asyncEdh masterThread $ perEntity ent [] ews
    launchTx [] txws
  launchTx ((ent, ers) : txrs) txws@(_ : _) = do
    case w of
      Nothing          -> asyncEdh masterThread $ perEntity ent ers []
      Just (_ent, ews) -> asyncEdh masterThread $ perEntity ent ers ews
    launchTx txrs txws'
    where (w, txws') = takeOutFromList ((== ent) . fst) txws

  perEntity
    :: Entity
    -> [(AttrKey, MVar EdhValue)]
    -> [(AttrKey, MVar EdhValue)]
    -> IO ()
  perEntity ent ers ews = modifyMVar_ ent $ \e -> do
    forM_ ers $ \(key, vv) -> case Map.lookup key e of
      Nothing -> error "bug in attr resolution"
      Just v  -> putMVar vv v
    flip Map.union e . Map.fromList <$> forM
      ews
      (\(key, vv) -> (key, ) <$> readMVar vv)

