
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
        | LetStmt ArgsReceiver ArgsSender
        | ClassStmt ClassName ProcDecl
        | ExtendsStmt Expr
        | MethodStmt MethodName ProcDecl
        | WhileStmt Expr StmtSrc
        | BreakStmt | ContinueStmt
        | OpDeclStmt OpSymbol Precedence ProcDecl
        | OpOvrdStmt OpSymbol ProcDecl
        | TryStmt {
            try'trunk :: !StmtSrc
            , try'catches :: ![(Expr, Maybe AttrName, StmtSrc)]
            , try'finally :: !(Maybe StmtSrc)
            }
        | YieldStmt Expr
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
                , if'consequence :: !Expr
                , if'alternative :: !(Maybe Expr)
                }
        | ListExpr [Expr]
        | DictExpr [(Expr, Expr)]
        | TupleExpr [Expr]

        -- a sequence of exprs in a pair of parentheses,
        -- optionally separated by semcolon,
        -- need this as curly braces have been used for
        -- dict expr, this works like a block statement,
        -- but should eval to last expr's value.
        -- and further an AST inspector can tell if a single
        -- expr is in parentheses from this.
        | GroupExpr [Expr]

        | LitExpr Literal

        | ForExpr ArgsReceiver Expr Expr
        | GeneratorExpr ProcDecl

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
