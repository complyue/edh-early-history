
-- | Abstract Syntax Tree of the Edh language
module Language.Edh.AST where

import           Prelude
import           Data.Text                     as T
import           Data.Lossless.Decimal          ( Decimal )

type ModulePath = FilePath

data Module = Module ModulePath SeqStmts
    deriving (Show)

type SeqStmts = [Stmt]

data Stmt = ImportStmt ArgsReceiver Expr
            | ClassStmt AttrName ProcDecl
            | ExtendsStmt Expr
            | MethodStmt AttrName ProcDecl
            | OpDeclStmt OpSymbol Precedence ProcDecl
            | OpOvrdStmt OpSymbol ProcDecl
            | ReturnStmt Expr
            | TryStmt { try'trunk :: Stmt
                    , try'catches :: [(Expr, Maybe AttrName, Stmt)]
                    , try'finally :: Maybe Stmt
                    }
            | BlockStmt [Stmt]
            | ExprStmt Expr
            | AssignStmt AttrRef Expr
    deriving (Show)

data ProcDecl = ProcDecl { fn'args :: ArgsReceiver
                    ,  fn'body :: Stmt
                    }
    deriving (Show)

data ArgsReceiver = WildReceiver | ArgsReceiver [ArgReceiver]
    deriving (Show)

data ArgReceiver = RecvRestArgs AttrName
            | RecvArg AttrName (Maybe AttrName) (Maybe Expr)
    deriving (Show)

data AttrRef = ThisRef | SupersRef
            | DirectRef AttrName
            | IndirectRef Expr AttrName
    deriving (Show)

type AttrName = Text

type OpSymbol = Text

type Precedence = Int

prec'Call :: Int
prec'Call = 10

prec'Index :: Int
prec'Index = 10

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
