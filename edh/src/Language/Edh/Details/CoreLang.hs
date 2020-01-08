
-- | core language functionalities
module Language.Edh.Details.CoreLang where

import           Prelude
-- import           Debug.Trace

import           Control.Monad

import           Control.Concurrent.STM

import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map
import qualified Data.List.NonEmpty            as NE

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Details.RtTypes


-- | resolve an attribute addressor, either alphanumeric named or symbolic
resolveAddr :: EdhProgState -> AttrAddressor -> STM AttrKey
resolveAddr _ (NamedAttr !attrName) = return (AttrByName attrName)
resolveAddr !pgs (SymbolicAttr !symName) =
  let scope = contextScope $ edh'context pgs
  in  resolveEdhCtxAttr scope (AttrByName symName) >>= \case
        Just scope' -> do
          em <- readTVar (scopeEntity scope')
          case Map.lookup (AttrByName symName) em of
            Just (EdhSymbol !symVal) -> return (AttrBySym symVal)
            Just v ->
              throwEdhSTM pgs EvalError
                $  "Not a symbol as "
                <> symName
                <> ", it is a "
                <> T.pack (show $ edhTypeOf v)
                <> ": "
                <> T.pack (show v)
            Nothing -> error "bug in ctx attr resolving"
        Nothing ->
          throwEdhSTM pgs EvalError
            $  "No symbol named "
            <> T.pack (show symName)
            <> " available"


-- * Edh attribute resolution


lookupEdhCtxAttr :: Scope -> AttrKey -> STM (Maybe EdhValue)
lookupEdhCtxAttr fromScope addr = resolveEdhCtxAttr fromScope addr >>= \case
  Nothing                 -> return Nothing
  Just (Scope !ent _ _ _) -> do
    em <- readTVar ent
    return $ Map.lookup addr em

lookupEdhObjAttr :: Object -> AttrKey -> STM (Maybe EdhValue)
lookupEdhObjAttr this addr = resolveEdhObjAttr this addr >>= \case
  Nothing                 -> return Nothing
  Just (Scope !ent _ _ _) -> do
    em <- readTVar ent
    return $ Map.lookup addr em


resolveEdhCtxAttr :: Scope -> AttrKey -> STM (Maybe Scope)
resolveEdhCtxAttr scope@(Scope !ent _ lexi'stack _) !addr =
  readTVar ent >>= \em -> if Map.member addr em
    then -- directly present on current scope
         return (Just scope)
    else resolveLexicalAttr lexi'stack addr

resolveLexicalAttr :: [Scope] -> AttrKey -> STM (Maybe Scope)
resolveLexicalAttr [] _ = return Nothing
resolveLexicalAttr (scope@(Scope !ent !obj _ _) : outerScopes) addr =
  readTVar ent >>= \em -> if Map.member addr em
    then -- directly present on current scope
         return (Just scope)
    else -- go for the interesting attribute from inheritance hierarchy
         -- of this context object, so a module as an object, can `extends`
         -- some objects too, in addition to the `import` mechanism
      (if ent == objEntity obj
          -- go directly to supers as entity has just got negative result
          then readTVar (objSupers obj) >>= resolveEdhSuperAttr addr
          -- context scope is different entity from this context object,
          -- start next from this object
          else resolveEdhObjAttr obj addr
        )
        >>= \case
              Just scope'from'object -> return $ Just scope'from'object
              -- go one level outer of the lexical stack
              Nothing                -> resolveLexicalAttr outerScopes addr

resolveEdhObjAttr :: Object -> AttrKey -> STM (Maybe Scope)
resolveEdhObjAttr !this !addr = readTVar thisEnt >>= \em ->
  if Map.member addr em
    then return
      (Just $ Scope thisEnt
                    this
                    (NE.toList $ classLexiStack $ objClass this)
                    clsProc
      )
    else readTVar (objSupers this) >>= resolveEdhSuperAttr addr
 where
  !thisEnt = objEntity this
  clsProc  = classProcedure (objClass this)

resolveEdhSuperAttr :: AttrKey -> [Object] -> STM (Maybe Scope)
resolveEdhSuperAttr _ [] = return Nothing
resolveEdhSuperAttr !addr (super : restSupers) =
  resolveEdhObjAttr super addr >>= \case
    Just scope -> return $ Just scope
    Nothing    -> resolveEdhSuperAttr addr restSupers


resolveEdhInstance :: Class -> Object -> STM (Maybe Object)
resolveEdhInstance class_ this = if objClass this == class_
  then return (Just this)
  else readTVar (objSupers this) >>= msum . (resolveEdhInstance class_ <$>)
