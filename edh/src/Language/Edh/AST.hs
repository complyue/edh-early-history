
-- | Abstract Syntax Tree of the Edh language
module Language.Edh.AST where

import           Prelude

import           Data.Text                      ( Text )

import           Data.Lossless.Decimal          ( Decimal )

import           Text.Megaparsec


type OpSymbol = Text
type AttrName = Text
data AttrAddressor =
    -- | vanilla form in addressing attributes against
    --   a left hand entity object
    NamedAttr !AttrName
    -- | get the symbol value from current entity,
    --   then use it to address attributes against
    --   a left hand entity object
    | SymbolicAttr !AttrName
  deriving (Show)


type ModuleId = FilePath
type ClassName = AttrName
type MethodName = AttrName


type SeqStmts = [StmtSrc]


newtype StmtSrc = StmtSrc (SourcePos, Stmt)
instance Eq StmtSrc where
  StmtSrc (x'sp, _) == StmtSrc (y'sp, _) = x'sp == y'sp
instance Show StmtSrc where
  show (StmtSrc (sp, _)) = "Edh statement @ " ++ sourcePosPretty sp


data Stmt = VoidStmt
    | ImportStmt !ArgsReceiver !Expr
    | LetStmt !ArgsReceiver !ArgsSender
    | ClassStmt !ClassName !ProcDecl
    | ExtendsStmt !Expr
    | MethodStmt !MethodName !ProcDecl
    | WhileStmt !Expr !StmtSrc
    | BreakStmt | ContinueStmt
    | FallthroughStmt
    | OpDeclStmt !OpSymbol !Precedence !ProcDecl
    | OpOvrdStmt !OpSymbol !ProcDecl
    | TryStmt {
        try'trunk :: !StmtSrc
        , try'catches :: ![(Expr, Maybe AttrName, StmtSrc)]
        , try'finally :: !(Maybe StmtSrc)
        }
    | YieldStmt !Expr
    | ReturnStmt !Expr
    | ExprStmt !Expr
  deriving (Show)

data AttrAddr = ThisRef | SupersRef
    | DirectRef !AttrAddressor
    | IndirectRef !Expr !AttrAddressor
  deriving (Show)

data ArgsReceiver = PackReceiver ![ArgReceiver]
    | SingleReceiver !ArgReceiver
    | WildReceiver
  deriving (Show)
data ArgReceiver = RecvRestPosArgs !AttrName
    | RecvRestKwArgs !AttrName
    | RecvRestPkArgs !AttrName
    | RecvArg !AttrName !(Maybe AttrAddr) !(Maybe Expr)
  deriving (Show)

data ArgsSender = PackSender ![ArgSender]
    | SingleSender !ArgSender
  deriving (Show)
data ArgSender = UnpackPosArgs !Expr
    | UnpackKwArgs !Expr
    | SendPosArg !Expr
    | SendKwArg !AttrName !Expr
  deriving (Show)

data ProcDecl = ProcDecl { procedure'args :: !ArgsReceiver
                        ,  procedure'body :: !StmtSrc
                        }
    deriving (Show)

data Prefix = PrefixPlus | PrefixMinus | Not
    | Guard -- similar to guard in Haskell
    | AtoIso -- atomically isolated
    | Go | Defer -- similar to goroutine in Go
  deriving (Show)

data Expr = LitExpr !Literal | PrefixExpr !Prefix !Expr
    | IfExpr { if'condition :: !Expr
            , if'consequence :: !StmtSrc
            , if'alternative :: !(Maybe StmtSrc)
            }
    | CaseExpr { case'target :: !Expr , case'branches :: !StmtSrc }

    | DictExpr ![Expr] -- should all be Infix ":"
    | ListExpr ![Expr]
    | TupleExpr ![Expr]
    | ParenExpr !Expr

    -- a block is an expression in Edh, instead of a statement as in
    -- a C family language. it evaluates to the value of last expr
    -- within it, in case no `EdhCaseClose` encountered, or can stop
    -- early with the value from a `EdhCaseClose`, typically returned
    -- from the branch `(->)` operator.
    --
    -- this is used to implement the case-of construct, as well as
    -- to allow multiple statements grouped as a single
    -- expression fitting into subclauses of if-then-else,
    -- while, and for-from-do constructs.
    | BlockExpr ![StmtSrc]

    | ForExpr !ArgsReceiver !Expr !Expr
    | GeneratorExpr !SourcePos !ProcDecl

    | AttrExpr !AttrAddr
    | IndexExpr { index'value :: !Expr
                , index'target :: !Expr
                }
    | CallExpr !Expr !ArgsSender

    | InfixExpr !OpSymbol !Expr !Expr
  deriving (Show)


data Literal = NilLiteral
    | DecLiteral !Decimal
    | BoolLiteral !Bool
    | StringLiteral !Text
    | SinkCtor -- sink constructor
    | TypeLiteral !EdhTypeValue
  deriving (Show)


type Precedence = Int


-- | the type for the value of type of a value,
-- a type name should parse as literals, so here it is.
data EdhTypeValue = TypeType
    -- nil has no type, its type if you really ask, is nil
    | DecimalType
    | BoolType
    | StringType
    | SymbolType
    | ObjectType
    | ModuleType
    | DictType
    | ListType
    | PairType
    | TupleType
    | ArgsPackType
    | BlockType
    | ThunkType
    | HostProcType
    | ClassType
    | MethodType
    | GeneratorType
    | FlowCtrlType -- for break/continue/fallthrough/yield/return
    | IteratorType
    | SinkType
    | ProxyType
  deriving (Eq, Ord, Show)

