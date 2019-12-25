
module Language.Edh.Batteries.Reflect where

import           Prelude
-- import           Debug.Trace

import           Control.Monad.Reader
import           Control.Concurrent.STM

import qualified Data.List.NonEmpty            as NE
import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Runtime


-- | utility scope()
-- obtain current scope as reflected object
scopeObtainProc :: EdhProcedure
scopeObtainProc _ _ _ !exit = do
  !pgs <- ask
  let (      Context !world !stack) = edh'context pgs
      scope@(Scope !ent this _sp  ) = NE.head stack

  exitEdhProc exit (this, scope, EdhObject (Object ent (scopeClass world) []))


-- | utility scope.eval(expr1, expr2, kw3=expr3, kw4=expr4, ...)
-- evaluate expressions in this scope
scopeEvalProc :: EdhProcedure
scopeEvalProc !argsSender _ _ !exit = undefined


-- | utility scope.extract(an1, an2, kw3=an3, kw4=an4, ...)
-- extract attribute values from this scope
scopeExtractProc :: EdhProcedure
scopeExtractProc !argsSender _ (Scope !scopeEnt !scopeObj _) !exit = do
  !pgs <- ask
  let !callerCtx   = edh'context pgs
      !callerScope = contextScope callerCtx

  undefined


-- | utility scope.implant(kw1=val1, kw2=val2)
-- implant attribute values into this scope
scopeImplantProc :: EdhProcedure
scopeImplantProc !argsSender _ _ !exit = undefined


