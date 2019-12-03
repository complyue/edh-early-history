module Parser.AST where

import           RIO

import qualified Data.Text                     as T

import qualified GHC.Show                      as G

newtype Program = Program BlockStmt
                deriving (Show, Eq)

data Stmt = LetStmt Ident Expr
          | ReturnStmt Expr
          | ExprStmt Expr
          deriving (Show, Eq)

type BlockStmt = [Stmt]

data Expr = IdentExpr Ident
          | LitExpr Literal
          | PrefixExpr Prefix Expr
          | InfixExpr Infix Expr Expr
          | IfExpr { cond :: Expr
                   , consequence :: BlockStmt
                   , alternative :: Maybe BlockStmt
                   }
          | FnExpr { params :: [Ident]
                   , body :: BlockStmt
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

data Literal = IntLiteral Integer
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
           deriving (Show, Eq)

data Precedence = PLowest
                | PEquals
                | PLessGreater
                | PSum
                | PProduct
                | PCall
                | PIndex
                deriving (Show, Eq, Ord)
