
-- | Abstract Syntax Tree of the Edh language
module Language.Edh.AST where

import           Prelude

import           Data.Text                     as T
import           Data.Lossless.Decimal          ( Decimal )

import           Text.Megaparsec                ( SourcePos )


type OpSymbol = Text
type AttrName = Text
data AttrAddressor =
        -- | vanilla form in addressing attributes against
        --   a left hand entity object
        NamedAttr AttrName
        -- | get the symbol value from current entity,
        --   then use it to address attributes against
        --   a left hand entity object
        | SymbolicAttr AttrName
    deriving (Show)


type ModuleId = FilePath
type ClassName = AttrName
type MethodName = AttrName


type SeqStmts = [StmtSrc]

type StmtSrc = (SourcePos, Stmt)

data Stmt = ImportStmt ArgsReceiver Expr
        | AssignStmt ArgsReceiver ArgsSender
        | ClassStmt ClassName ProcDecl
        | ExtendsStmt Expr
        | MethodStmt MethodName ProcDecl
        -- TODO for loop should serve a callback mechanism from hosting
        --      language/runtime, by allowing sophiscated implementions
        --      as the expr. yet this idea is to be validated further.
        | ForStmt ArgsReceiver Expr StmtSrc
        | WhileStmt Expr StmtSrc
        | BreakStmt | ContinueStmt
        | OpDeclStmt OpSymbol Precedence ProcDecl
        | OpOvrdStmt OpSymbol ProcDecl
        | TryStmt {
            try'trunk :: !StmtSrc
            , try'catches :: ![(Expr, Maybe AttrName, StmtSrc)]
            , try'finally :: !(Maybe StmtSrc)
            }
        | BlockStmt SeqStmts
        | ReturnStmt Expr
        | ExprStmt Expr
    deriving (Show)

data AttrAddr = ThisRef | SupersRef
            | DirectRef AttrAddressor
            | IndirectRef Expr AttrAddressor
    deriving (Show)

data ArgsReceiver = PackReceiver [ArgReceiver]
        | SingleReceiver ArgReceiver
        | WildReceiver
    deriving (Show)
data ArgReceiver = RecvRestPosArgs AttrName
        | RecvRestKwArgs AttrName
        | RecvArg AttrName (Maybe AttrAddr) (Maybe Expr)
    deriving (Show)

data ArgsSender = PackSender [ArgSender]
        | SingleSender ArgSender
    deriving (Show)
data ArgSender = UnpackPosArgs Expr
        | UnpackKwArgs Expr
        | SendPosArg Expr
        | SendKwArg AttrName Expr
    deriving (Show)

data ProcDecl = ProcDecl { fn'args :: !ArgsReceiver
                        ,  fn'body :: !StmtSrc
                        }
    deriving (Show)

data Prefix = PrefixPlus | PrefixMinus | Not
    deriving (Show)

data Expr = PrefixExpr Prefix Expr
            | IfExpr { if'condition :: !Expr
                    , if'consequence :: !StmtSrc
                    , if'alternative :: !(Maybe StmtSrc)
                    }
            | ListExpr [Expr]
            | DictExpr [(Expr, Expr)]
            | LitExpr Literal
            | AttrExpr AttrAddr
            | CallExpr Expr ArgsSender
            | IndexExpr { index'value :: !Expr
                        , index'target :: !Expr
                        }
            | InfixExpr OpSymbol Expr Expr
    deriving (Show)

data Literal = DecLiteral Decimal
            | BoolLiteral Bool
            | StringLiteral Text
    deriving (Show)


type Precedence = Int
