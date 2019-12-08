{-# LANGUAGE BlockArguments #-}

module Language.Edh.Parser where

import           Prelude

import           Text.Megaparsec

import           Language.Edh.AST

import           Language.Edh.Parser.Details


parseModule :: ModulePath -> Parser Module
parseModule pth = Module pth <$> (sc >> many parseStmt)

