
-- | Abstract Syntax Tree of the Edh language
module Language.Edh.AST where

import           Prelude
import           Data.Text                     as T
import           Data.Lossless.Decimal          ( Decimal )

type ModulePath = FilePath

data Module = Module ModulePath SeqStmts
    deriving (Show)

type SeqStmts = [Stmt]

data Stmt = ImportStmt CallReceiver Expr
            | ClassStmt AttrName FnDecl
            | ExtendsStmt Expr
            | MethodStmt AttrName FnDecl
            | ReturnStmt Expr
            | TryStmt { try'trunk :: Stmt
                    , try'catches :: [(Expr, Maybe AttrName, Stmt)]
                    , try'finally :: Maybe Stmt
                    }
            | BlockStmt [Stmt]
            | OpDeclStmt OpSymbol Precedence FnDecl
            | OpOvrdStmt OpSymbol FnDecl
            | ExprStmt Expr
            | AssignStmt AttrRef Expr
    deriving (Show)

data FnDecl = FnDecl { fn'args :: CallReceiver
                    ,  fn'body :: Stmt
                    }
    deriving (Show)

data CallReceiver = WildReceiver | CallReceiver [ArgReceiver]
    deriving (Show)

data ArgReceiver = RecvArg AttrName (Maybe Expr)
                    | RecvRestArgs AttrName
    deriving (Show)

data AttrRef = ThisRef
            | DirectRef AttrName
            | IndirectRef Expr AttrName
    deriving (Show)

type AttrName = Text

type OpSymbol = Text

type Precedence = Int

precCall :: Int
precCall = 10

precIndex :: Int
precIndex = 10

data Expr = PrefixExpr Prefix Expr
            | IfExpr { if'condition :: Expr
                    , if'consequence :: Stmt
                    , if'alternative :: Maybe Stmt
                    }
            | ListExpr [Expr]
            | DictExpr [(Expr, Expr)]
            | LitExpr Literal
            | AttrExpr AttrRef
            | CallExpr Expr [ArgSender]
            | IndexExpr { index'value :: Expr
                        , index'target :: Expr
                        }
            | InfixExpr Infix Expr Expr
    deriving (Show)

data ArgSender = SendPosArg Expr
                | SendNamedArg AttrName Expr
    deriving (Show)

data Literal = DecLiteral Decimal
            | BoolLiteral Bool
            | StringLiteral Text
    deriving (Show)

data Prefix = PrefixPlus | PrefixMinus | Not
    deriving (Show)

data Infix = Infix OpSymbol Precedence
    deriving (Show)
