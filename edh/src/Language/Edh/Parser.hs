{-# LANGUAGE BlockArguments #-}

module Language.Edh.Parser where

import           Prelude

import           Control.Applicative     hiding ( many
                                                , some
                                                )
import           Data.Text                     as T
import           Data.Void

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Language.Edh.AST

import           Language.Edh.Parser.Details


parseModule :: ModulePath -> Parser Module
parseModule pth = Module pth <$> (sc >> many parseStmt)

parseAssign :: Parser Stmt
parseAssign = try do
    ar <- parseAttrRef
    symbol "="
    expr <- parseExpr
    case attrRef of
        ThisRef -> parseError "can not assign to this"
        _       -> return $ AssignStmt ar expr


