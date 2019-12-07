
-- | Abstract Syntax Tree of the Edh language
module Language.Edh.AST where

import           Prelude
import           Data.Text                     as T
import           Data.Scientific

type ModulePath = FilePath

data Module = Module ModulePath SeqStmts
    deriving (Show)

type SeqStmts = [Stmt]

data Stmt = ImportStmt CallReceiver ImportSource
            | AssignStmt AttrRef Expr
            | OperatorDeclStmt OpSymbol Precedence FnDecl
            | OperatorOvrdStmt OpSymbol FnDecl
            | ReturnStmt Expr
            | BlockStmt [Stmt]
            | ExprStmt Expr
    deriving (Show)

data CallReceiver = WildReceiver | CallReceiver [ArgReceiver]
    deriving (Show)

data ArgReceiver = RecvPosArg AttrId
                    | RecvRestArgs AttrId
                    | RecvOptArg AttrId Expr
                    | RecvWildArgs
    deriving (Show)

data ImportSource = ImpFromModule ModulePath
                    | ImpFromObject Expr
    deriving (Show)

data AttrRef = ThisRef
            | NamedRef AttrId
            | RefPath [AttrRef]
    deriving (Show)

data AttrId = Attribute AttrName | Operator OpSymbol
    deriving (Eq, Ord)

type AttrName = Text
type OpSymbol = Text

instance Show AttrId where
    show (Attribute a ) = T.unpack a
    show (Operator  op) = "(" <> T.unpack op <> ")"

type Precedence = Int

precCall :: Int
precCall = 10

precIndex :: Int
precIndex = 10

data Expr = LitExpr Literal
            | AttrExpr AttrRef
            | PrefixExpr Prefix Expr
            | InfixExpr Infix Expr Expr
            | IfExpr { if'condition :: Expr
                    , if'consequence :: Stmt
                    , if'alternative :: Maybe Stmt
                    }
            | TryExpr { try'trunk :: Stmt
                    , try'catches :: [(Expr, Maybe AttrId, Stmt)]
                    , try'finally :: Maybe Stmt
                    }
            | ClassFnDecl FnDecl
            | MethodFnDecl FnDecl
            | CallExpr Expr [ArgSender]
            | ListExpr [Expr]
            | DictExpr [(Expr, Expr)]
            | IndexExpr { index'value :: Expr
                        , index'target :: Expr
                        }
    deriving (Show)

data FnDecl = FnDecl { fn'args :: CallReceiver
                    ,  fn'body :: SeqStmts
                    }
    deriving (Show)

data ArgSender = SendPosArg Expr
                | SendNamedArg AttrId Expr
    deriving (Show)

data Literal = DecLiteral Scientific
            | BoolLiteral Bool
            | StringLiteral Text
    deriving (Show)

data Prefix = PrefixPlus | PrefixMinus | Not
    deriving (Show)

data Infix = Infix OpSymbol Precedence
    deriving (Show)
