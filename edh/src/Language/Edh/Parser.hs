{-# LANGUAGE BlockArguments #-}

module Language.Edh.Parser where

import           Prelude                       as Prelude

import           Control.Monad.State.Strict

import           Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

import           Language.Edh.AST
import           Language.Edh.Parser.Details


declareOperators :: Text -> [(OpSymbol, Precedence)] -> Parser ()
declareOperators srcLoc opd = do
    opPD <- get
    put $ Map.unionWithKey chkCompatible opPD $ Map.fromList $ flip
        Prelude.map
        opd
        \(op, p) -> (op, (p, srcLoc))
  where
    chkCompatible
        :: OpSymbol
        -> (Precedence, Text)
        -> (Precedence, Text)
        -> (Precedence, Text)
    chkCompatible op (p1, t1) (p2, _) = if p1 /= p2
        then error
            (  "invalid precedence change "
            <> show p1
            <> " -> "
            <> show p2
            <> " for operator: "
            <> show op
            )
        else (p1, t1)


parseModule :: Parser SeqStmts
parseModule = sc *> many parseStmt

