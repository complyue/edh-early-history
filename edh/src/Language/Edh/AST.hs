
-- | Abstract Syntax Tree of the Edh language
module Language.Edh.AST where

import           Prelude
import           Data.Text                     as T
import           Data.Lossless.Decimal          ( Decimal )

type ModulePath = FilePath
type AttrName = Text
type OpSymbol = Text

data Module = Module ModulePath SeqStmts
    deriving (Show)

type SeqStmts = [Stmt]

data Stmt = ImportStmt ArgsReceiver Expr
            | AssignStmt ArgsReceiver ArgsSender
            | ClassStmt AttrName ProcDecl
            | ExtendsStmt Expr
            | MethodStmt AttrName ProcDecl
            | ForStmt ArgsReceiver Expr Stmt
            | OpDeclStmt OpSymbol Precedence ProcDecl
            | OpOvrdStmt OpSymbol ProcDecl
            | TryStmt { try'trunk :: Stmt
                        , try'catches :: [(Expr, Maybe AttrName, Stmt)]
                        , try'finally :: Maybe Stmt
                        }
            | BlockStmt [Stmt]
            | ReturnStmt Expr
            | ExprStmt Expr
    deriving (Show)

data AttrRef = ThisRef | SupersRef
            | DirectRef AttrName
            | IndirectRef Expr AttrName
    deriving (Show)

data ArgsReceiver = PackReceiver [ArgReceiver]
        | SingleReceiver ArgReceiver
        | WildReceiver
    deriving (Show)
data ArgReceiver = RecvRestPosArgs AttrName
        | RecvRestKwArgs AttrName
        | RecvArg AttrName (Maybe AttrRef) (Maybe Expr)
    deriving (Show)

data ArgsSender = PackSender [ArgSender]
        | SingleSender ArgSender
    deriving (Show)
data ArgSender = UnpackPosArgs Expr
        | UnpackKwArgs Expr
        | SendPosArg Expr
        | SendKwArg AttrName Expr
    deriving (Show)

data ProcDecl = ProcDecl { fn'args :: ArgsReceiver
                        ,  fn'body :: Stmt
                        }
    deriving (Show)

data Prefix = PrefixPlus | PrefixMinus | Not
    deriving (Show)

data Expr = PrefixExpr Prefix Expr
            | IfExpr { if'condition :: Expr
                    , if'consequence :: Stmt
                    , if'alternative :: Maybe Stmt
                    }
            | ListExpr [Expr]
            | DictExpr [(Expr, Expr)]
            | LitExpr Literal
            | AttrExpr AttrRef
            | CallExpr Expr ArgsSender
            | IndexExpr { index'value :: Expr
                        , index'target :: Expr
                        }
            | InfixExpr OpSymbol Expr Expr
    deriving (Show)

data Literal = DecLiteral Decimal
            | BoolLiteral Bool
            | StringLiteral Text
    deriving (Show)


type Precedence = Int
