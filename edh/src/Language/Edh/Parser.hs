{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Language.Edh.Parser where

import           Prelude

import           Control.Applicative     hiding ( many
                                                , some
                                                )
import           Control.Monad
import           Control.Monad.State.Strict
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

trailingColon :: Parser ()
trailingColon = void $ optional $ symbol ";"


isLetter :: Char -> Bool
isLetter = flip elem $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']

isIdentChar :: Char -> Bool
isIdentChar c = isLetter c || isDigit c || c == '\''

isDigit :: Char -> Bool
isDigit = flip elem ['0' .. '9']

isOperatorChar :: Char -> Bool
isOperatorChar = flip elem ("=~!@#$%^&|:<>?+-*/" :: [Char])


parseProgram :: Parser SeqStmts
parseProgram = sc *> many parseStmt <* eof

parseVoidStmt :: Parser Stmt
parseVoidStmt = VoidStmt <$ symbol "pass" -- same as Python

parseImportStmt :: Parser Stmt
parseImportStmt = do
    void $ symbol "import"
    ir     <- parseArgsReceiver
    impSrc <- parseExpr
    return $ ImportStmt ir impSrc

parseLetStmt :: Parser Stmt
parseLetStmt = do
    void $ symbol "let"
    receiver <- parseArgsReceiver
    void $ symbol "="
    sender <- parseArgsSender
    return $ LetStmt receiver sender

parseArgsReceiver :: Parser ArgsReceiver
parseArgsReceiver =
    (symbol "*" *> return WildReceiver) <|> parsePackReceiver <|> do
        singleArg <- parseKwRecv False
        return $ SingleReceiver singleArg

parsePackReceiver :: Parser ArgsReceiver
parsePackReceiver = between
    (symbol "(")
    (symbol ")")
    do
        argRs <- parseArgRecvs [] False False
        return $ PackReceiver $ reverse argRs

parseArgRecvs :: [ArgReceiver] -> Bool -> Bool -> Parser [ArgReceiver]
parseArgRecvs rs kwConsumed posConsumed =
    (lookAhead (symbol ")") >> return rs) <|> do
        nextArg <-
            (if posConsumed then parseKwRecv True else nextPosArg)
                <* trailingComma
        case nextArg of
            RecvRestPosArgs _ -> parseArgRecvs (nextArg : rs) kwConsumed True
            RecvRestKwArgs _ -> parseArgRecvs (nextArg : rs) True posConsumed
            _ -> parseArgRecvs (nextArg : rs) kwConsumed posConsumed
  where
    nextPosArg, restKwArgs, restPosArgs :: Parser ArgReceiver
    nextPosArg = restKwArgs <|> restPosArgs <|> parseKwRecv True
    restKwArgs = do
        void $ symbol "**"
        aname <- parseAttrName
        return $ RecvRestKwArgs aname
    restPosArgs = do
        void $ symbol "*"
        aname <- parseAttrName
        return $ RecvRestPosArgs aname

parseRetarget :: Parser AttrAddr
parseRetarget = do
    void $ symbol "as"
    retgt <- parseAttrAddr
    return retgt

parseArgLetExpr :: Parser Expr
parseArgLetExpr = do
    void $ symbol "="
    valExpr <- parseExpr
    return valExpr

parseKwRecv :: Bool -> Parser ArgReceiver
parseKwRecv inPack = do
    aname   <- parseAttrName
    retgt   <- optional parseRetarget
    defExpr <- if inPack then optional parseArgLetExpr else return Nothing
    return $ RecvArg aname (validateTgt retgt) defExpr
  where
    validateTgt :: Maybe AttrAddr -> Maybe AttrAddr
    validateTgt tgt = case tgt of
        Nothing        -> Nothing
        Just ThisRef   -> fail "can not overwrite this"
        Just SupersRef -> fail "can not overwrite supers"
        _              -> tgt


parseSupersRef :: Parser AttrAddr
parseSupersRef = SupersRef <$ symbol "supers"

parseAttrAddr :: Parser AttrAddr
parseAttrAddr =
    -- `supers` is a list thus can not be further addressed
    -- for attribute access, can stop at it on sight
                parseSupersRef <|> do
    p1 <- leadingPart
    moreAddr p1
  where
    leadingPart :: Parser Expr
    leadingPart = choice
        [ (AttrExpr ThisRef) <$ symbol "this"
        , (AttrExpr . DirectRef . SymbolicAttr) <$> parseAttrSym
        , (AttrExpr . DirectRef . NamedAttr) <$> parseAttrName
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
                    let r1 = IndirectRef p1 addr
                    in  moreAddr (AttrExpr r1) <|> return r1
                _ -> error "bug"
            )
            <|> case p1 of
                    AttrExpr ThisRef -> return ThisRef
                    AttrExpr r1      -> return r1
                    _expr            -> error "bug"


parseArgsSender :: Parser ArgsSender
parseArgsSender = parsePackSender <|> do
    expr <- parseExpr
    return $ SingleSender $ SendPosArg expr

parsePackSender :: Parser ArgsSender
parsePackSender = between
    (symbol "(")
    (symbol ")")
    do
        argSs <- parseArgSends []
        return $ PackSender $ reverse argSs

parseArgSends :: [ArgSender] -> Parser [ArgSender]
parseArgSends ss = (lookAhead (symbol ")") >> return ss) <|> do
    arg <- nextArg <* trailingComma
    parseArgSends $ arg : ss
  where
    nextArg, unpackKwArgs, unpackPosArgs :: Parser ArgSender
    nextArg      = unpackKwArgs <|> unpackPosArgs <|> parseKwSend
    unpackKwArgs = do
        void $ symbol "**"
        expr <- parseExpr
        return $ UnpackKwArgs expr
    unpackPosArgs = do
        void $ symbol "*"
        expr <- parseExpr
        return $ UnpackPosArgs expr
    parseKwSend :: Parser ArgSender
    parseKwSend = do
        p1 <- parseExpr
        optional parseArgLetExpr >>= \case
            Nothing      -> return $ SendPosArg p1
            Just valExpr -> case p1 of
                AttrExpr (DirectRef (NamedAttr aname)) ->
                    return $ SendKwArg aname valExpr
                _ -> fail $ "invalid argument name: " <> show p1


parseClassStmt :: Parser Stmt
parseClassStmt = do
    void $ symbol "class"
    cname    <- parseAlphaName
    procDecl <- parseProcDecl
    return $ ClassStmt cname procDecl

parseExtendsStmt :: Parser Stmt
parseExtendsStmt = do
    void $ symbol "extends"
    superExpr <- parseExpr
    return $ ExtendsStmt superExpr

parseMethodStmt :: Parser Stmt
parseMethodStmt = do
    void $ symbol "method"
    mname    <- parseAlphaName
    procDecl <- parseProcDecl
    return $ MethodStmt mname procDecl

parseWhileStmt :: Parser Stmt
parseWhileStmt = do
    void $ symbol "while"
    cond <- parseExpr
    stmt <- parseStmt
    return $ WhileStmt cond stmt

parseProcDecl :: Parser ProcDecl
parseProcDecl = do
    cr   <- parseArgsReceiver
    body <- parseStmt
    return $ ProcDecl cr body

parseOpDeclOvrdStmt :: Parser Stmt
parseOpDeclOvrdStmt = do
    void $ symbol "operator"
    srcLoc   <- getSourcePos
    opSym    <- parseOpLit
    precDecl <- optional $ L.decimal <* sc
    procDecl <- parseProcDecl
    opPD     <- get
    case precDecl of
        Nothing -> do
            case Map.lookup opSym opPD of
                Nothing -> fail $ "undeclared operator: " <> T.unpack opSym
                _       -> return ()
            return $ OpOvrdStmt opSym procDecl
        Just opPrec -> do
            if opPrec < 0 || opPrec >= 10
                then (fail $ "invalid operator precedence: " <> show opPrec)
                else return ()
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
    final   <- optional do
        void $ symbol "finally"
        parseStmt
    return $ TryStmt trunk catches final
  where
    parseCatch = do
        void $ symbol "catch"
        excClass <- parseExpr
        an       <- optional do
            void $ symbol "as"
            parseAttrName
        recov <- parseStmt
        return (excClass, an, recov)

parseYieldStmt :: Parser Stmt
parseYieldStmt = do
    void $ symbol "yield"
    asend <- parseArgsSender
    return $ YieldStmt asend

parseReturnStmt :: Parser Stmt
parseReturnStmt = do
    void $ symbol "return"
    expr <- parseExpr
    return $ ReturnStmt expr


parseStmt :: Parser StmtSrc
parseStmt = do
    srcPos <- getSourcePos
    ((,) srcPos)
        <$> choice
                [ parseImportStmt
                , parseClassStmt
                , parseExtendsStmt
                , parseMethodStmt
                , parseWhileStmt
                -- TODO validate break/continue must within a loop seque
                , BreakStmt <$ symbol "break"
                , ContinueStmt <$ symbol "continue"
                -- TODO validate fallthrough must within a case-of seque
                , FallthroughStmt <$ symbol "fallthrough"
                , parseOpDeclOvrdStmt
                , parseTryStmt
                -- TODO validate yield must within a generator procedure
                , parseYieldStmt
                , parseReturnStmt
                , parseVoidStmt
                , ExprStmt <$> parseExpr
                ]
        <*  trailingColon


parsePrefixExpr :: Parser Expr
parsePrefixExpr = choice
    [ PrefixExpr PrefixPlus <$> (symbol "+" *> parseExpr)
    , PrefixExpr PrefixMinus <$> (symbol "-" *> parseExpr)
    , PrefixExpr Not <$> (symbol "not" >> parseExpr)
    -- guard precedence should be no smaller than the branch op (->)
    , (symbol "|" >> parseExprPrec 1 (PrefixExpr Guard))
    , PrefixExpr Go <$> (symbol "go" >> requireCallOrSeque)
    , PrefixExpr Defer <$> (symbol "defer" >> requireCallOrSeque)
    ]
  where
    requireCallOrSeque = do
        o <- getOffset
        e <- parseExpr
        case e of
            ce@(CallExpr _ _) -> return ce
            se@(SequeExpr _) -> return se
            _ -> setOffset o >> fail "a call/seque required here"

parseIfExpr :: Parser Expr
parseIfExpr = do
    void $ symbol "if"
    cond <- parseExpr
    void $ symbol "then"
    cseq <- parseStmt
    alt  <- optional do
        void $ symbol "else"
        parseStmt
    return $ IfExpr cond cseq alt

parseCaseExpr :: Parser Expr
parseCaseExpr = do
    void $ symbol "case"
    tgt <- parseExpr
    void $ symbol "of"
    seque <- parseStmt
    return $ CaseExpr tgt seque

parseForExpr :: Parser Expr
parseForExpr = do
    void $ symbol "for"
    ar <- parseArgsReceiver
    void $ symbol "from"
    iter <- parseExpr
    void $ symbol "do"
    act <- parseExpr
    return $ ForExpr ar iter act

parseGeneratorExpr :: Parser Expr
parseGeneratorExpr = do
    void $ symbol "generator"
    srcPos   <- getSourcePos
    procDecl <- parseProcDecl
    return $ GeneratorExpr srcPos procDecl

parseListExpr :: Parser Expr
parseListExpr =
    ListExpr
        <$> ( between (symbol "[") (symbol "]")
            $ many (parseExpr <* trailingComma)
            )

parseDictExpr :: Parser Expr
parseDictExpr =
    DictExpr <$> (between (symbol "{") (symbol "}") $ many parseDictPair)
  where
    parseDictPair = do
        keyExpr <- parseDictKey
        void $ symbol ":"
        valExpr <- parseExpr
        trailingComma
        return (keyExpr, valExpr)
    -- colon is ambiguous in here, for now:
    -- simply only allow literal and attribute addressor for dict key
    -- todo improve this by a operator accepting parser, which treats
    -- colon specially ?
    parseDictKey = (LitExpr <$> parseLitExpr) <|> (AttrExpr <$> parseAttrAddr)

parseStringLit :: Parser Text
parseStringLit = lexeme do
    delim <- char '\"' <|> char '\'' <|> char '`'
    T.pack <$> manyTill L.charLiteral (char delim)

parseBoolLit :: Parser Bool
parseBoolLit =
    (symbol "true" *> return True) <|> (symbol "false" *> return False)

parseDecLit :: Parser Decimal
parseDecLit = lexeme do -- todo support HEX/OCT ?
    sn <- L.signed (return ()) L.scientific
    return $ Decimal 1 (fromIntegral $ base10Exponent sn) (coefficient sn)

parseLitExpr :: Parser Literal
parseLitExpr = choice
    [ NilLiteral <$ litSym "nil"
    , BoolLiteral <$> parseBoolLit
    , StringLiteral <$> parseStringLit
    , ChanCtor <$ litSym "chan"
    , DecLiteral D.nan <$ litSym "nan"
    , DecLiteral D.inf <$ litSym "inf"
    , DecLiteral <$> parseDecLit

    -- todo use template-haskell here to avoid manual sync with 'EdhTypeValue'
    , TypeLiteral DecimalType <$ litSym "DecimalType"
    , TypeLiteral BoolType <$ litSym "BoolType"
    , TypeLiteral StringType <$ litSym "StringType"
    , TypeLiteral SymbolType <$ litSym "SymbolType"
    , TypeLiteral ObjectType <$ litSym "ObjectType"
    , TypeLiteral ModuleType <$ litSym "ModuleType"
    , TypeLiteral DictType <$ litSym "DictType"
    , TypeLiteral ListType <$ litSym "ListType"
    , TypeLiteral TupleType <$ litSym "TupleType"
    , TypeLiteral SequeType <$ litSym "SequeType"
    , TypeLiteral ThunkType <$ litSym "ThunkType"
    , TypeLiteral HostProcType <$ litSym "HostProcType"
    , TypeLiteral ClassType <$ litSym "ClassType"
    , TypeLiteral MethodType <$ litSym "MethodType"
    , TypeLiteral GeneratorType <$ litSym "GeneratorType"
    , TypeLiteral FlowCtrlType <$ litSym "FlowCtrlType"
    , TypeLiteral IteratorType <$ litSym "IteratorType"
    , TypeLiteral ChannelType <$ litSym "ChannelType"
    , TypeLiteral ProxyType <$ litSym "ProxyType"
    , TypeLiteral TypeType <$ litSym "TypeType"
    ]
    where litSym = hidden . symbol


parseAttrName :: Parser Text
parseAttrName = parseOpName <|> parseAlphaName

parseAttrSym :: Parser AttrName
parseAttrSym = char '@' *> parseAlphaName

parseAlphaName :: Parser AttrName
parseAlphaName = lexeme do
    anStart <- takeWhile1P (Just "attribute name") isLetter
    anRest  <- takeWhileP Nothing isIdentChar
    return $ anStart <> anRest

parseOpName :: Parser Text
parseOpName = between (symbol "(") (symbol ")") parseOpLit

parseOpLit :: Parser Text
parseOpLit = lexeme $ takeWhile1P (Just "operator symbol") isOperatorChar


parseIndexExpr :: Parser Expr
parseIndexExpr = between (symbol "[") (symbol "]") parseExpr

parseTupleOrSeque :: Parser Expr
parseTupleOrSeque = choice [(try $ parseSeque), parseTuple]

parseSeque :: Parser Expr
parseSeque = symbol "(" *> (notFollowedBy $ symbol ",") *> parseSequeRest []
  where
    parseSequeRest :: [StmtSrc] -> Parser Expr
    parseSequeRest t = (optional $ symbol ";") *> choice
        [ ((symbol ")") *> (return $ SequeExpr (reverse t)))
        , (do
              srcPos <- getSourcePos
              e      <- parseExpr
              parseSequeRest $ (srcPos, ExprStmt e) : t
          )
        ]

parseTuple :: Parser Expr
parseTuple = symbol "(" *> parseTupleRest []
  where
    parseTupleRest :: [Expr] -> Parser Expr
    parseTupleRest t = (optional $ symbol ",") *> choice
        [ (symbol ")") *> (return $ TupleExpr (reverse t))
        , parseExpr >>= \e -> parseTupleRest $ e : t
        ]


parseExpr :: Parser Expr
parseExpr = parseExprPrec 0 id

-- prefix operators are hardcoded, only three: + - not

-- other operators must be infix binary operators, they can be declared
-- and/or overridden everywhere, while they are left assosciative only

parseExprPrec :: Precedence -> (Expr -> Expr) -> Parser Expr
parseExprPrec prec leftCtor = choice
    [ parsNonIdxNonCallPrec prec leftCtor
    , parseIdxNonCallPrec prec leftCtor
    , parseIdxCallPrec prec leftCtor
    ]

parsNonIdxNonCallPrec :: Precedence -> (Expr -> Expr) -> Parser Expr
parsNonIdxNonCallPrec prec leftCtor = do
    e1 <- choice
        [ -- non-idexable, non-callable exprs
          parseIfExpr
        , parseCaseExpr
        , parseForExpr
        , parsePrefixExpr -- can only be bool or decimal
        , LitExpr <$> parseLitExpr
        ]
    parseNextOp e1 prec leftCtor

parseIdxNonCallPrec :: Precedence -> (Expr -> Expr) -> Parser Expr
parseIdxNonCallPrec prec leftCtor = do
    e1 <- choice
        [ -- possibly indexable, non-callable exprs
          AttrExpr <$> parseSupersRef
        , parseListExpr
        , parseDictExpr
        , parseTupleOrSeque
        ]
    optional parseIndexExpr >>= \case
        Just idxVal -> parseNextOp (IndexExpr idxVal e1) prec leftCtor
        Nothing     -> parseNextOp e1 prec leftCtor

parseIdxCallPrec :: Precedence -> (Expr -> Expr) -> Parser Expr
parseIdxCallPrec prec leftCtor = do
    addrExpr <- choice
        [ -- possibly callable exprs
          parseGeneratorExpr
        , AttrExpr <$> parseAttrAddr
        ]
    (optional $ lookAhead $ (symbol ",") <|> (symbol ";")) >>= \case
        Just _  -> return $ leftCtor addrExpr
        Nothing -> do
            optional parseIndexExpr >>= \case
                Just idxVal ->
                    parseNextOp (IndexExpr idxVal addrExpr) prec leftCtor
                Nothing -> optional parsePackSender >>= \case
                    Just packSender ->
                        parseNextOp (CallExpr addrExpr packSender) prec leftCtor
                    Nothing -> parseNextOp addrExpr prec leftCtor

parseNextOp :: Expr -> Precedence -> (Expr -> Expr) -> Parser Expr
parseNextOp e1 prec leftCtor = do
    optional parseOpLit >>= \case
        Nothing    -> return $ leftCtor e1
        Just opSym -> do
            opPD <- get
            case Map.lookup opSym opPD of
                Nothing -> fail $ "undeclared operator: " <> T.unpack opSym
                Just (opPrec, _opDeclPos) ->
                    parseExprPrec opPrec $ \nextExpr -> if prec < opPrec
                        then leftCtor $ InfixExpr opSym e1 nextExpr
                        else InfixExpr opSym (leftCtor e1) nextExpr

