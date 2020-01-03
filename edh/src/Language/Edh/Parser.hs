
module Language.Edh.Parser where

import           Prelude

import           Control.Applicative     hiding ( many
                                                , some
                                                )
import           Control.Monad
import           Control.Monad.State.Strict

import           Data.Functor
import qualified Data.Char                     as Char
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Scientific
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Data.Lossless.Decimal         as D

import           Language.Edh.Control
import           Language.Edh.AST


sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

trailingComma :: Parser ()
trailingComma = void $ optional $ symbol ","

optionalSemicolon :: Parser ()
optionalSemicolon = void $ optional $ symbol ";"


isLetter :: Char -> Bool
isLetter = flip elem $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']

isIdentChar :: Char -> Bool
isIdentChar c = isLetter c || isDigit c || c == '\''

isDigit :: Char -> Bool
isDigit = flip elem ['0' .. '9']

isOperatorChar :: Char -> Bool
isOperatorChar c = if c > toEnum 128
  then Char.isSymbol c
  else elem c ("=~!@#$%^&|:<>?+-*/" :: [Char])

parseProgram :: Parser SeqStmts
parseProgram = sc *> many parseStmt <* eof

parseVoidStmt :: Parser Stmt
parseVoidStmt = VoidStmt <$ symbol "pass" -- same as Python

parseImportStmt :: Parser Stmt
parseImportStmt = do
  void $ symbol "import"
  liftA2 ImportStmt parseArgsReceiver parseExpr

parseLetStmt :: Parser Stmt
parseLetStmt = do
  void $ symbol "let"
  receiver <- parseArgsReceiver
  void $ symbol "="
  LetStmt receiver <$> parseArgsSender

parseArgsReceiver :: Parser ArgsReceiver
parseArgsReceiver =
  (symbol "*" *> return WildReceiver) <|> parsePackReceiver <|> do
    singleArg <- parseKwRecv False
    return $ SingleReceiver singleArg

parsePackReceiver :: Parser ArgsReceiver
parsePackReceiver = between
  (symbol "(")
  (symbol ")")
  (parseArgRecvs [] False False >>= return . PackReceiver . reverse)

parseArgRecvs :: [ArgReceiver] -> Bool -> Bool -> Parser [ArgReceiver]
parseArgRecvs rs kwConsumed posConsumed =
  (lookAhead (symbol ")") >> return rs) <|> do
    nextArg <-
      (if posConsumed
          then restPkArgs <|> restKwArgs <|> parseKwRecv True
          else nextPosArg
        )
        <* trailingComma
    case nextArg of
      RecvRestPosArgs _ -> parseArgRecvs (nextArg : rs) kwConsumed True
      RecvRestKwArgs  _ -> parseArgRecvs (nextArg : rs) True posConsumed
      _                 -> parseArgRecvs (nextArg : rs) kwConsumed posConsumed
 where
  nextPosArg, restKwArgs, restPosArgs :: Parser ArgReceiver
  nextPosArg = restPkArgs <|> restKwArgs <|> restPosArgs <|> parseKwRecv True
  restPkArgs = do
    void $ symbol "***"
    RecvRestPkArgs <$> parseAttrName
  restKwArgs = do
    void $ symbol "**"
    RecvRestKwArgs <$> parseAttrName
  restPosArgs = do
    void $ symbol "*"
    RecvRestPosArgs <$> parseAttrName

parseRetarget :: Parser AttrAddr
parseRetarget = do
  void $ symbol "as"
  parseAttrAddr

parseArgLetExpr :: Parser Expr
parseArgLetExpr = do
  void $ symbol "="
  parseExpr

parseKwRecv :: Bool -> Parser ArgReceiver
parseKwRecv inPack = do
  aname   <- parseAttrName
  retgt   <- optional parseRetarget
  defExpr <- if inPack then optional parseArgLetExpr else return Nothing
  return $ RecvArg aname (validateTgt retgt) defExpr
 where
  validateTgt :: Maybe AttrAddr -> Maybe AttrAddr
  validateTgt tgt = case tgt of
    Nothing      -> Nothing
    Just ThisRef -> fail "can not overwrite this"
    Just ThatRef -> fail "can not overwrite that"
    _            -> tgt


parseAttrAddr :: Parser AttrAddr
parseAttrAddr = do
  p1 <- leadingPart
  moreAddr p1
 where
  leadingPart :: Parser Expr
  leadingPart = choice
    [ AttrExpr ThisRef <$ symbol "this"
    , AttrExpr ThatRef <$ symbol "that"
    , AttrExpr . DirectRef . SymbolicAttr <$> parseAttrSym
    , AttrExpr . DirectRef . NamedAttr <$> parseAttrName
    ]
  followingPart :: Parser Expr
  followingPart = choice
    [ (symbol "this") *> fail "invalid this reference"
    , (AttrExpr . DirectRef . SymbolicAttr) <$> parseAttrSym
    , (AttrExpr . DirectRef . NamedAttr) <$> parseAttrName
    ]
  moreAddr :: Expr -> Parser AttrAddr
  moreAddr p1 =
    (symbol "." *> followingPart >>= \case
        AttrExpr (DirectRef addr) ->
          let r1 = IndirectRef p1 addr in moreAddr (AttrExpr r1) <|> return r1
        _ -> error "bug"
      )
      <|> case p1 of
            AttrExpr ThisRef -> return ThisRef
            AttrExpr ThatRef -> return ThatRef
            AttrExpr r1      -> return r1
            _                -> error "bug"


parseArgsSender :: Parser ArgsSender
parseArgsSender = parsePackSender <|> do
  SingleSender . SendPosArg <$> parseExpr

parsePackSender :: Parser ArgsSender
parsePackSender =
  between (symbol "(") (symbol ")") $ PackSender . reverse <$> parseArgSends []

parseArgSends :: [ArgSender] -> Parser [ArgSender]
parseArgSends ss = (lookAhead (symbol ")") >> return ss) <|> do
  arg <- nextArg <* trailingComma
  parseArgSends $ arg : ss
 where
  nextArg, unpackPkArgs, unpackKwArgs, unpackPosArgs :: Parser ArgSender
  nextArg      = unpackPkArgs <|> unpackKwArgs <|> unpackPosArgs <|> parseKwSend
  unpackPkArgs = do
    void $ symbol "***"
    UnpackPkArgs <$> parseExpr
  unpackKwArgs = do
    void $ symbol "**"
    UnpackKwArgs <$> parseExpr
  unpackPosArgs = do
    void $ symbol "*"
    UnpackPosArgs <$> parseExpr
  parseKwSend :: Parser ArgSender
  parseKwSend = do
    o <- getOffset
    parseExpr >>= \case
      InfixExpr "=" nExpr vExpr -> case nExpr of
        AttrExpr (DirectRef (NamedAttr attrName)) ->
          return $ SendKwArg attrName vExpr
        _ -> do
          setOffset o
          fail $ "invalid argument name: " <> show nExpr
      vExpr -> return $ SendPosArg vExpr


parseClassStmt :: Parser Stmt
parseClassStmt = do
  void $ symbol "class"
  ClassStmt <$> parseProcDecl

parseExtendsStmt :: Parser Stmt
parseExtendsStmt = do
  void $ symbol "extends"
  ExtendsStmt <$> parseExpr

parseMethodStmt :: Parser Stmt
parseMethodStmt = do
  void $ symbol "method"
  MethodStmt <$> parseProcDecl

parseGeneratorStmt :: Parser Stmt
parseGeneratorStmt = do
  void $ symbol "generator"
  GeneratorStmt <$> parseProcDecl

parseWhileStmt :: Parser Stmt
parseWhileStmt = do
  void $ symbol "while"
  liftA2 WhileStmt parseExpr parseStmt

parseProcDecl :: Parser ProcDecl
parseProcDecl = liftA3 ProcDecl
                       (parseMagicProcName <|> parseAlphaName)
                       parseArgsReceiver
                       parseStmt

parseMagicProcName :: Parser Text
parseMagicProcName = between (symbol "(") (symbol ")") $ lexeme $ takeWhile1P
  (Just "magic procedure name")
  isMagicProcChar

-- to allow magic method names like ([]) ([=]) etc.
isMagicProcChar :: Char -> Bool
isMagicProcChar c = isOperatorChar c || elem c ("[]" :: [Char])

parseOpDeclOvrdStmt :: Parser Stmt
parseOpDeclOvrdStmt = do
  void $ symbol "operator"
  srcLoc   <- getSourcePos
  opSym    <- parseOpLit
  precDecl <- optional $ L.decimal <* sc
  -- todo restrict forms of valid args receiver for operators, e.g. 
  --  * 2 pos-args - simple lh/rh value receiving operator
  --  * 3 pos-args - caller scope + lh/rh expr receiving operator
  argRcvr  <- parseArgsReceiver
  body     <- parseStmt
  let procDecl = ProcDecl opSym argRcvr body
  opPD <- get
  case precDecl of
    Nothing -> case Map.lookup opSym opPD of
      Nothing          -> fail $ "undeclared operator: " <> T.unpack opSym
      Just (opPrec, _) -> return $ OpOvrdStmt opSym procDecl opPrec
    Just opPrec -> do
      when (opPrec < 0 || opPrec >= 10)
           (fail $ "invalid operator precedence: " <> show opPrec)
      case Map.lookup opSym opPD of
        Nothing -> return ()
        Just (_, odl) ->
          fail
            $  "redeclaring operator: "
            <> T.unpack opSym
            <> " which has been declared at: "
            <> T.unpack odl
      put $ Map.insert opSym (opPrec, T.pack $ show srcLoc) opPD
      return $ OpDeclStmt opSym opPrec procDecl

parseTryStmt :: Parser Stmt
parseTryStmt = do
  void $ symbol "try"
  trunk   <- parseStmt
  catches <- many parseCatch
  final   <- optional $ do
    void $ symbol "finally"
    parseStmt
  return $ TryStmt trunk catches final
 where
  parseCatch = do
    void $ symbol "catch"
    excClass <- parseExpr
    an       <- optional $ do
      void $ symbol "as"
      parseAttrName
    recov <- parseStmt
    return (excClass, an, recov)

parseReturnStmt :: Parser Stmt
parseReturnStmt = do
  void $ symbol "return"
  ReturnStmt <$> parseExpr

parseThrowStmt :: Parser Stmt
parseThrowStmt = do
  void $ symbol "throw"
  ThrowStmt <$> parseExpr


parseStmt :: Parser StmtSrc
parseStmt = optionalSemicolon *> do
  srcPos <- getSourcePos
  StmtSrc
    .   (srcPos, )
    <$> choice
          [ parseImportStmt
          , parseLetStmt
          , parseClassStmt
          , parseExtendsStmt
          , parseMethodStmt
          , parseGeneratorStmt
          , parseWhileStmt
    -- TODO validate break/continue must within a loop construct
          , BreakStmt <$ symbol "break"
          , ContinueStmt <$ symbol "continue"
    -- TODO validate fallthrough must within a case-of block
          , FallthroughStmt <$ symbol "fallthrough"
          , parseOpDeclOvrdStmt
          , parseTryStmt
    -- TODO validate yield must within a generator procedure
          , parseReturnStmt
          , parseThrowStmt
          , parseVoidStmt
          , ExprStmt <$> parseExpr
          ]
    <*  optionalSemicolon


parseIfExpr :: Parser Expr
parseIfExpr = do
  void $ symbol "if"
  cond <- parseExpr
  void $ symbol "then"
  cseq <- parseStmt
  alt  <- optional $ do
    void $ symbol "else"
    parseStmt
  return $ IfExpr cond cseq alt

parseCaseExpr :: Parser Expr
parseCaseExpr = do
  void $ symbol "case"
  tgt <- parseExpr
  void $ symbol "of"
  CaseExpr tgt <$> parseStmt

parseYieldExpr :: Parser Expr
parseYieldExpr = do
  void $ symbol "yield"
  YieldExpr <$> parseExpr

parseForExpr :: Parser Expr
parseForExpr = do
  void $ symbol "for"
  ar <- parseArgsReceiver
  void $ symbol "from"
  iter <- parseExpr
  void $ symbol "do"
  ForExpr ar iter <$> parseExpr

parseListExpr :: Parser Expr
parseListExpr = ListExpr
  <$> between (symbol "[") (symbol "]") (many (parseExpr <* trailingComma))

parseStringLit :: Parser Text
parseStringLit = lexeme $ do
  delim <- char '\"' <|> char '\'' <|> char '`'
  T.pack <$> manyTill L.charLiteral (char delim)

parseBoolLit :: Parser Bool
parseBoolLit =
  (symbol "true" *> return True) <|> (symbol "false" *> return False)

parseDecLit :: Parser Decimal
parseDecLit = lexeme $ do -- todo support HEX/OCT ?
  sn <- L.signed (return ()) L.scientific
  return $ Decimal 1 (fromIntegral $ base10Exponent sn) (coefficient sn)

parseLitExpr :: Parser Literal
parseLitExpr = choice
  [ NilLiteral <$ litSym "nil"
  , BoolLiteral <$> parseBoolLit
  , StringLiteral <$> parseStringLit
  , SinkCtor <$ litSym "sink"
  , DecLiteral D.nan <$ litSym "nan"
  , DecLiteral D.inf <$ litSym "inf"
  , DecLiteral <$> parseDecLit

    -- todo use template-haskell here to avoid manual sync with 'EdhTypeValue'
  , TypeLiteral DecimalType <$ litSym "DecimalType"
  , TypeLiteral BoolType <$ litSym "BoolType"
  , TypeLiteral StringType <$ litSym "StringType"
  , TypeLiteral SymbolType <$ litSym "SymbolType"
  , TypeLiteral ObjectType <$ litSym "ObjectType"
  , TypeLiteral DictType <$ litSym "DictType"
  , TypeLiteral ListType <$ litSym "ListType"
  , TypeLiteral TupleType <$ litSym "TupleType"
  , TypeLiteral BlockType <$ litSym "BlockType"
  , TypeLiteral ThunkType <$ litSym "ThunkType"
  , TypeLiteral HostProcType <$ litSym "HostProcType"
  , TypeLiteral ClassType <$ litSym "ClassType"
  , TypeLiteral MethodType <$ litSym "MethodType"
  , TypeLiteral OperatorType <$ litSym "OperatorType"
  , TypeLiteral GeneratorType <$ litSym "GeneratorType"
  , TypeLiteral FlowCtrlType <$ litSym "FlowCtrlType"
  , TypeLiteral GenrIterType <$ litSym "GenrIterType"
  , TypeLiteral SinkType <$ litSym "SinkType"
  , TypeLiteral ExprType <$ litSym "ExprType"
  , TypeLiteral TypeType <$ litSym "TypeType"
  ]
  where litSym = hidden . symbol


parseAttrName :: Parser Text
parseAttrName = parseOpName <|> parseAlphaName

parseAttrSym :: Parser AttrName
parseAttrSym = char '@' *> parseAlphaName

parseAlphaName :: Parser AttrName
parseAlphaName = lexeme $ do
  anStart <- takeWhile1P (Just "attribute name") isLetter
  anRest  <- takeWhileP Nothing isIdentChar
  return $ anStart <> anRest

parseOpName :: Parser Text
parseOpName = between (symbol "(") (symbol ")") parseOpLit

parseOpLit :: Parser Text
parseOpLit = lexeme $ takeWhile1P (Just "operator symbol") isOperatorChar

parseBlockOrDict :: Parser Expr
parseBlockOrDict = choice [try parseBlock, parseDict]

parseBlock :: Parser Expr
parseBlock =
  symbol "{" *> notFollowedBy (symbol ",") *> parseBlockRest False []
 where
  parseBlockRest :: Bool -> [StmtSrc] -> Parser Expr
  parseBlockRest mustBlock t = do
    mustBlock' <- optional (symbol ";") >>= \case
      Nothing -> return mustBlock
      _       -> return True
    choice
      [ symbol "}"
        $> (case t of
             [] | mustBlock' -> BlockExpr []
          -- let {} parse as empty dict instead of empty block
             []              -> DictExpr []
          -- single k:v pair without comma will reach here
             [StmtSrc (_, ExprStmt pairExpr@(InfixExpr ":" _ _))] ->
               DictExpr [pairExpr]
             _ -> BlockExpr (reverse t)
           )
      , do
        ss <- parseStmt
        notFollowedBy (symbol ":")
        parseBlockRest mustBlock' $ ss : t
      ]

parseDict :: Parser Expr
parseDict = symbol "{" *> parseDictRest []
 where
  parseDictRest :: [Expr] -> Parser Expr
  parseDictRest t = optional (symbol ",") *> choice
    [ symbol "}" $> DictExpr (reverse t)
    , parseKeyValPair >>= \p -> parseDictRest $ p : t
    ]
  parseKeyValPair :: Parser Expr
  parseKeyValPair = do
    pairExpr <- parseExpr
    trailingComma
    case pairExpr of
      InfixExpr ":" _ _ -> return pairExpr
      _                 -> fail $ "Invalid dict entry: " <> show pairExpr


parseOpAddrOrTupleOrParen :: Parser Expr
parseOpAddrOrTupleOrParen =
  symbol "("
    *> (   (AttrExpr . DirectRef . NamedAttr <$> (parseOpLit <* symbol ")"))
       <|> parseTupleRest ")" False []
       )

parseTupleRest :: Text -> Bool -> [Expr] -> Parser Expr
parseTupleRest closeSym mustTuple t = do
  mustTuple' <- optional (symbol ",") >>= \case
    Nothing -> return mustTuple
    _       -> return True
  choice
    [ symbol closeSym
      $> (case t of
           [singleExpr] | not mustTuple' ->
             if closeSym == ")" then ParenExpr singleExpr else singleExpr
           _ -> TupleExpr (reverse t)
         )
    , parseExpr >>= \e -> parseTupleRest closeSym mustTuple' $ e : t
    ]

parseIndexer :: Parser Expr
parseIndexer = symbol "[" *> parseTupleRest "]" False []


-- Notes:
--  * (+)/(-) prefix should have highest precedence below Call/Index
--  * (not) should have a precedence slightly higher than (&&) (||)
--  * guard (|) should have a precedence no smaller than the branch op (->)
--  * (ai) should have lowest precedence
--  * (go)/(defer) can apply only to a call or loop

parsePrefixExpr :: Parser Expr
parsePrefixExpr = choice
  [ (symbol "+" >> notFollowedBy (satisfy isOperatorChar))
  >>  PrefixExpr PrefixPlus
  <$> parseExprPrec 9
  , (symbol "-" >> notFollowedBy (satisfy isOperatorChar))
  >>  PrefixExpr PrefixMinus
  <$> parseExprPrec 9
  , symbol "not" >> PrefixExpr Not <$> parseExprPrec 4
  , (symbol "|" >> notFollowedBy (satisfy isOperatorChar))
  >>  PrefixExpr Guard
  <$> parseExprPrec 1
  , PrefixExpr AtoIso <$> (symbol "ai" >> parseExpr)
  , PrefixExpr Go <$> (symbol "go" >> requireCallOrLoop)
  , PrefixExpr Defer <$> (symbol "defer" >> requireCallOrLoop)
  ]
 where
  requireCallOrLoop = do
    o <- getOffset
    e <- parseExpr
    case e of
      ce@CallExpr{} -> return ce
      le@ForExpr{}  -> return le
      _             -> setOffset o >> fail "a call/for required here"


-- besides hardcoded prefix operators, all other operators are infix binary
-- operators, they can be declared and further overridden everywhere,
-- while they are left-assosciative only

parseExprPrec :: Precedence -> Parser Expr
parseExprPrec prec =
  choice
      [ parsePrefixExpr
      , parseYieldExpr
      , parseForExpr
      , parseIfExpr
      , parseCaseExpr
      , parseListExpr
      , parseBlockOrDict
      , parseOpAddrOrTupleOrParen
      , LitExpr <$> parseLitExpr
      , AttrExpr <$> parseAttrAddr
      ]
    >>= parseMoreOps
 where
  parseMoreOps :: Expr -> Parser Expr
  parseMoreOps expr = choice
    [ parseIndexer >>= parseMoreOps . flip IndexExpr expr
    , parsePackSender >>= parseMoreOps . CallExpr expr
    , parseMoreInfix expr
    ]
  parseMoreInfix :: Expr -> Parser Expr
  parseMoreInfix leftExpr = higherOp prec >>= \case
    Nothing -> return leftExpr
    Just (opPrec, opSym) ->
      parseExprPrec opPrec >>= parseMoreInfix . InfixExpr opSym leftExpr

  higherOp :: Precedence -> Parser (Maybe (Precedence, OpSymbol))
  higherOp prec' = do
    beforeOp <- getParserState
    optional parseOpLit >>= \case
      Nothing    -> return Nothing
      Just opSym -> do
        opPD <- get
        case Map.lookup opSym opPD of
          Nothing -> fail $ "undeclared operator: (" <> T.unpack opSym <> ")"
          Just (opPrec, _) -> if opPrec > prec'
            then return $ Just (opPrec, opSym)
            else do
              -- leave this op to be encountered later, i.e.
              -- after left-hand expr collapsed into one
              setParserState beforeOp
              return Nothing


parseExpr :: Parser Expr
parseExpr = parseExprPrec (-1)
