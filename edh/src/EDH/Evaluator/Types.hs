{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module EDH.Evaluator.Types where

import           RIO
import           RIO.State

import           Control.Monad.Except
import           Control.Monad.Trans.Class      ( MonadTrans(..) )

import           EDH.Evaluator.BuiltIns         ( builtIns )
import           EDH.Evaluator.Object
import           EDH.Parser.AST                 ( )


newtype EvalError = EvalError Text
                    deriving (Show, Eq, Typeable)

instance Exception EvalError

newtype EvalState = EvalState EnvRef

getEnvRef :: Monad m => EvaluatorT m EnvRef
getEnvRef = do
    EvalState ref <- get
    return ref

setEnvRef :: Monad m => EnvRef -> EvaluatorT m ()
setEnvRef ref = put $ EvalState ref

createEmptyState :: IO EvalState
createEmptyState = EvalState <$> (emptyEnv >>= flip wrapEnv builtIns)

newtype EvaluatorT m a = EvaluatorT
  { runEvaluatorT :: StateT EvalState (ExceptT EvalError m) a }

instance Functor m => Functor (EvaluatorT m) where
    fmap f (EvaluatorT e) = EvaluatorT $ fmap f e

instance Monad m => Applicative (EvaluatorT m) where
    pure = EvaluatorT . pure
    EvaluatorT mf <*> EvaluatorT ma = EvaluatorT $ mf <*> ma

instance Monad m => Monad (EvaluatorT m) where
    EvaluatorT ma >>= f = EvaluatorT $ ma >>= runEvaluatorT . f

instance Monad m => MonadState EvalState (EvaluatorT m) where
    get = EvaluatorT get
    put = EvaluatorT . put

instance Monad m => MonadError EvalError (EvaluatorT m) where
    throwError = EvaluatorT . throwError
    EvaluatorT e `catchError` f =
        EvaluatorT $ e `catchError` (runEvaluatorT . f)

instance MonadTrans EvaluatorT where
    lift = EvaluatorT . lift . lift

type Evaluator = EvaluatorT IO

execEvaluatorT
    :: Monad m
    => EvaluatorT m a
    -> EvalState
    -> m (Either EvalError (a, EvalState))
execEvaluatorT = (runExceptT .) . runStateT . runEvaluatorT
