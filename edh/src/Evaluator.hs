{-# LANGUAGE ScopedTypeVariables #-}
module Evaluator where

import           RIO                     hiding ( Hashable )

import           Control.Monad.Except

import qualified Data.Map.Strict               as M
import           Evaluator.Object
import           Evaluator.Types
import           Parser.AST
import           Utils                          ( at )

evalProgram :: Program -> Evaluator Object
evalProgram (Program blockStmt) = returned <$> evalBlockStmt blockStmt

evalBlockStmt :: BlockStmt -> Evaluator Object
evalBlockStmt []       = return nil
evalBlockStmt (s : []) = evalStmt s
evalBlockStmt (s : ss) = do
    o <- evalStmt s
    if isReturned o then return o else evalBlockStmt ss

evalStmt :: Stmt -> Evaluator Object
evalStmt (ExprStmt   expr   ) = evalExpr expr
evalStmt (ReturnStmt expr   ) = ret <$> evalExpr expr
evalStmt (LetStmt ident expr) = evalExpr expr >>= registerIdent ident

registerIdent :: Ident -> Object -> Evaluator Object
registerIdent ident o = do
    ref <- getEnvRef
    lift $ insertVar ident o ref
    return o

evalError :: Text -> Evaluator a
evalError = throwError . EvalError

evalExpr :: Expr -> Evaluator Object
evalExpr (IdentExpr i                    ) = evalIdent i
evalExpr (LitExpr   l                    ) = evalLiteral l
evalExpr (PrefixExpr p e                 ) = evalPrefix p e
evalExpr (InfixExpr i    l     r         ) = evalInfix i l r
evalExpr (IfExpr    cond conse maybeAlter) = evalIf cond conse maybeAlter
evalExpr (FnExpr   params body           ) = evalFn params body
evalExpr (CallExpr fn     args           ) = evalCall fn args
evalExpr (ArrayExpr es                   ) = evalArray es
evalExpr (HashExpr  hs                   ) = evalHash hs
evalExpr (IndexExpr a i                  ) = evalIndex a i

evalIdent :: Ident -> Evaluator Object
evalIdent i = do
    env <- getEnvRef
    var <- lift $ getVar i env
    case var of
        Just o  -> return o
        Nothing -> evalError $ "identifier not found: " <> tshow i

evalLiteral :: Literal -> Evaluator Object
evalLiteral (DecLiteral n e ) = return $ ODecimal $ Decimal e 1 n
evalLiteral (BoolLiteral   b) = return $ OBool b
evalLiteral (StringLiteral s) = return $ OString s

evalPrefix :: Prefix -> Expr -> Evaluator Object
evalPrefix Not         = fmap (OBool . not) . (evalExpr >=> o2b)
evalPrefix PrefixPlus  = fmap ODecimal . (evalExpr >=> o2n)
evalPrefix PrefixMinus = fmap (ODecimal . negateDecimal) . (evalExpr >=> o2n)

evalInfix :: Infix -> Expr -> Expr -> Evaluator Object
evalInfix Plus        = (join .) . ee2x oAdd return
evalInfix Minus       = (fmap ODecimal .) . ee2x subsDecimal o2n
evalInfix Multiply    = (fmap ODecimal .) . ee2x mulDecimal o2n
evalInfix Divide      = (fmap ODecimal .) . ee2x divDecimal o2n
evalInfix Eq          = (fmap OBool .) . ee2x (==) return
evalInfix NotEq       = (fmap OBool .) . ee2x (/=) return
evalInfix GreaterThan = (fmap OBool .) . ee2x decimalGreater o2n
evalInfix LessThan    = (fmap OBool .) . ee2x decimalLess o2n

oAdd :: Object -> Object -> Evaluator Object
oAdd (ODecimal x) (ODecimal y) = return . ODecimal $ addDecimal x y
oAdd (OString x) (OString y) = return . OString $ x <> y
oAdd x y = evalError $ tshow x <> " and " <> tshow y <> " are not addable"

evalIf :: Expr -> BlockStmt -> Maybe BlockStmt -> Evaluator Object
evalIf cond conse maybeAlter = do
    condBool <- evalExpr cond >>= o2b
    if condBool
        then evalBlockStmt conse
        else case maybeAlter of
            Just alter -> evalBlockStmt alter
            Nothing    -> return nil

evalFn :: [Ident] -> BlockStmt -> Evaluator Object
evalFn params body = do
    ref <- getEnvRef
    return $ OFn params body ref

evalCall :: Expr -> [Expr] -> Evaluator Object
evalCall fnExpr argExprs = do
    fn <- evalExpr fnExpr >>= o2f
    case fn of
        OFn        params body      fRef -> evalFnCall params body fRef
        OBuiltInFn _      numParams fn   -> evalBuiltInFnCall numParams fn
  where
    evalFnCall :: [Ident] -> BlockStmt -> EnvRef -> Evaluator Object
    evalFnCall params body fRef = do
        if length params /= length argExprs
            then
                evalError
                $  "wrong number of arguments: "
                <> tshow (length params)
                <> " expected but "
                <> tshow (length argExprs)
                <> " given"
            else do
                args    <- traverse evalExpr argExprs
                origRef <- getEnvRef
                lift (wrapEnv fRef $ zip params args) >>= setEnvRef
                o <- returned <$> evalBlockStmt body
                setEnvRef origRef
                return o

    evalBuiltInFnCall :: Int -> BuiltInFn -> Evaluator Object
    evalBuiltInFnCall numParams fn = do
        if numParams /= length argExprs
            then
                evalError
                $  "wrong number of arguments: "
                <> tshow (numParams)
                <> " expected but "
                <> tshow (length argExprs)
                <> " given"
            else do
                args <- traverse evalExpr argExprs
                res  <- lift $ fn args
                case res of
                    Left  t -> evalError t
                    Right o -> return o

evalArray :: [Expr] -> Evaluator Object
evalArray = fmap OArray . traverse evalExpr

evalHash :: [(Literal, Expr)] -> Evaluator Object
evalHash hs = do
    ps <- traverse evalPair hs
    return . OHash $ M.fromList ps
  where
    evalPair :: (Literal, Expr) -> Evaluator (Hashable, Object)
    evalPair (l, e) = do
        h <- l2h l
        o <- evalExpr e
        return (h, o)

decimalAsIndex :: Integral i => Decimal -> Evaluator i
decimalAsIndex o@(Decimal e d n)
    | d == 1 && e >= 0 = return $ fromIntegral n * 10 ^ e
    | otherwise        = evalError $ tshow o <> " is not a valid index"

evalIndex :: Expr -> Expr -> Evaluator Object
evalIndex targetE idxE = do
    target <- evalExpr targetE
    case target of
        OArray arr -> do
            (idx :: Integer) <- evalExpr idxE >>= o2n >>= decimalAsIndex
            return $ fromMaybe nil (arr `at` idx)
        OHash hash -> do
            h <- evalExpr idxE >>= o2h
            return $ M.findWithDefault nil h hash
        o -> evalError $ "unexpected index target: " <> tshow o

o2b :: Object -> Evaluator Bool
o2b (OBool b) = return b
o2b o         = evalError $ tshow o <> " is not a bool"

o2n :: Object -> Evaluator Decimal
o2n (ODecimal d) = return d
o2n o            = evalError $ tshow o <> " is not a number"

o2f :: Object -> Evaluator Object
o2f o@(OFn        _ _ _) = return o
o2f o@(OBuiltInFn _ _ _) = return o
o2f o                    = evalError $ tshow o <> " is not a function"

o2h :: Object -> Evaluator Hashable
o2h (ODecimal d) = return $ DecimalHash d
o2h (OBool    b) = return $ BoolHash b
o2h (OString  t) = return $ StringHash t
o2h o            = evalError $ tshow o <> " is not hashable"

l2h :: Literal -> Evaluator Hashable
l2h = evalLiteral >=> o2h

ee2x :: (a -> a -> b) -> (Object -> Evaluator a) -> Expr -> Expr -> Evaluator b
ee2x f = (liftM2 f `on`) . (evalExpr >=>)

eval :: Program -> IO (Either EvalError Object)
eval p = do
    s <- createEmptyState
    fmap fst <$> evalWithState p s

evalWithState
    :: Program -> EvalState -> IO (Either EvalError (Object, EvalState))
evalWithState p s = execEvaluatorT (evalProgram p) s
