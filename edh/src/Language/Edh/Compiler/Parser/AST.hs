module Language.Edh.Compiler.Parser.AST where

import           RIO

import qualified Data.Text                     as T

import qualified GHC.Show                      as G

newtype Program = Program SeqStmts
                deriving (Show, Eq)

data Stmt = AssignStmt Ident Expr
          | ReturnStmt Expr
          | ExprStmt Expr
          | BlockStmt [Stmt]
          deriving (Show, Eq)

type SeqStmts = [Stmt]

data Expr = IdentExpr Ident
          | LitExpr Literal
          | PrefixExpr Prefix Expr
          | InfixExpr Infix Expr Expr
          | IfExpr { cond :: Expr
                   , consequence :: Stmt
                   , alternative :: Maybe Stmt
                   }
          | FnExpr { params :: [Ident]
                   , body :: SeqStmts
                   }
          | CallExpr { function :: Expr
                     , arguments :: [Expr]
                     }
          | ArrayExpr [Expr]
          | HashExpr [(Literal, Expr)]
          | IndexExpr { array :: Expr
                      , index :: Expr
                      }
          deriving (Show, Eq)

data Literal = DecLiteral Integer Integer
             | BoolLiteral Bool
             | StringLiteral Text
             deriving (Show, Eq)

newtype Ident = Ident Text
              deriving (Eq, Ord)

instance G.Show Ident where
    show (Ident t) = T.unpack t

data Prefix = PrefixPlus | PrefixMinus | Not
            deriving (Show, Eq)

data Infix = Plus
           | Minus
           | Divide
           | Multiply
           | Eq
           | NotEq
           | GreaterThan
           | LessThan
           | UpdHash
           deriving (Show, Eq)

data Precedence = PLowest
                | PEquals
                | PLessGreater
                | PSum
                | PProduct
                | PHashUpd
                | PCall
                | PIndex
                deriving (Show, Eq, Ord)
