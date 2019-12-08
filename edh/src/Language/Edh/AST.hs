
-- | Abstract Syntax Tree of the Edh language
module Language.Edh.AST where

import           Prelude
import           Data.Text                     as T
import           Data.Lossless.Decimal          ( Decimal )

import           Text.Megaparsec


type ModulePath = FilePath
type AttrName = Text
type OpSymbol = Text

data Module = Module ModulePath SeqStmts
    deriving (Show)

type SeqStmts = [StmtSrc]

data StmtSrc = StmtSrc SourcePos Stmt
    deriving (Show)

data Stmt = ImportStmt ArgsReceiver Expr
        | AssignStmt ArgsReceiver ArgsSender
        | ClassStmt AttrName ProcDecl
        | ExtendsStmt Expr
        | MethodStmt AttrName ProcDecl
        | ForStmt ArgsReceiver Expr StmtSrc
        | OpDeclStmt OpSymbol Precedence ProcDecl
        | OpOvrdStmt OpSymbol ProcDecl
        | TryStmt {
            try'trunk :: StmtSrc
            , try'catches :: [(Expr, Maybe AttrName, StmtSrc)]
            , try'finally :: Maybe StmtSrc
            }
        | BlockStmt SeqStmts
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
                        ,  fn'body :: StmtSrc
                        }
    deriving (Show)

data Prefix = PrefixPlus | PrefixMinus | Not
    deriving (Show)

data Expr = PrefixExpr Prefix Expr
            | IfExpr { if'condition :: Expr
                    , if'consequence :: StmtSrc
                    , if'alternative :: Maybe StmtSrc
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
