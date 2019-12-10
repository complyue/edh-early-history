{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Language.Edh.Parser.Details where

import           Prelude

import           Control.Applicative     hiding ( many
                                                , some
                                                )
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void
import           Data.Scientific
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec         hiding ( State )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Data.Lossless.Decimal

import           Language.Edh.AST


-- use such a dict as the parsing state, to implement
-- object-language-declarable operator precendence
type OpPrecDict = Map.Map OpSymbol (Precedence, Text)

-- no backtracking needed for precedence dict, so it
-- can live in the inner monad of 'ParsecT'.
type Parser = ParsecT Void Text (State OpPrecDict)


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
                parseSupersRef <|> try do
    p1 <- firstPart
    nextAddr p1 <|> case p1 of
        AttrExpr r1 -> return r1
        _expr       -> error "bug"
  where
    firstPart :: Parser Expr
    firstPart = choice
        [ (AttrExpr ThisRef) <$ symbol "this"
        , (AttrExpr . DirectRef . SymbolicAttr) <$> parseAttrSym
        , (AttrExpr . DirectRef . NamedAttr) <$> parseAttrName
        ]
    nextPart :: Parser Expr
    nextPart = choice
        [ (AttrExpr . DirectRef . SymbolicAttr) <$> parseAttrSym
        , (AttrExpr . DirectRef . NamedAttr) <$> parseAttrName
        ]
    nextAddr :: Expr -> Parser AttrAddr
    nextAddr p1 = do
        void $ symbol "."
        nextPart >>= \case
            AttrExpr (DirectRef addr) -> return $ IndirectRef p1 addr
            _                         -> error "bug"


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
            srcLoc <- getSourcePos
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
    expr <- parseExpr
    return $ YieldStmt expr

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
                  -- TODO validate break/continue must within a loop block
                , BreakStmt <$ symbol "break"
                , ContinueStmt <$ symbol "continue"
                , parseOpDeclOvrdStmt
                , parseTryStmt
                -- TODO validate yield must within a generator function
                , parseYieldStmt
                , parseReturnStmt
                , ExprStmt <$> parseExpr
                ]
        <*  trailingColon


parsePrefixExpr :: Parser Expr
parsePrefixExpr = choice
    [ PrefixExpr PrefixPlus <$> (symbol "+" *> parseExpr)
    , PrefixExpr PrefixMinus <$> (symbol "-" *> parseExpr)
    , PrefixExpr Not <$> (symbol "not" >> parseExpr)
    ]

parseIfExpr :: Parser Expr
parseIfExpr = do
    void $ symbol "if"
    cond <- parseExpr
    void $ symbol "then"
    cseq <- parseExpr
    alt  <- optional do
        void $ symbol "else"
        parseExpr
    return $ IfExpr cond cseq alt

parseForExpr :: Parser Expr
parseForExpr = do
    void $ symbol "for"
    ar <- parseArgsReceiver
    void $ symbol "from"
    iter <- parseExpr
    void $ symbol "do"
    expr <- parseExpr
    return $ ForExpr ar iter expr

parseGeneratorExpr :: Parser Expr
parseGeneratorExpr = do
    void $ symbol "generator"
    procDecl <- parseProcDecl
    return $ GeneratorExpr procDecl

-- todo support list comprehension, e.g. [x for x from xs]
parseListExpr :: Parser Expr
parseListExpr =
    ListExpr
        <$> ( between (symbol "[") (symbol "]")
            $ many (parseExpr <* trailingComma)
            )

-- todo support dict comprehension, e.g. {k: v for (k, v) from ps}
parseDictExpr :: Parser Expr
parseDictExpr =
    DictExpr <$> (between (symbol "{") (symbol "}") $ many parseDictPair)
  where
    parseDictPair = do
        keyExpr <- parseExpr
        void $ symbol ":"
        valExpr <- parseExpr
        trailingComma
        return (keyExpr, valExpr)

parseStringLit :: Parser Text
parseStringLit = do
    delim <- char '\"' <|> char '\'' <|> char '`'
    T.pack <$> manyTill L.charLiteral (char delim)

parseBoolLit :: Parser Bool
parseBoolLit =
    (symbol "true" *> return True) <|> (symbol "false" *> return False)

parseDecLit :: Parser Decimal
parseDecLit = lexeme do -- TODO support HEX/OCT ?
    sn <- L.signed (return ()) L.scientific
    return $ Decimal 1 (fromIntegral $ base10Exponent sn) (coefficient sn)

parseLitExpr :: Parser Literal
parseLitExpr = choice
    [ StringLiteral <$> parseStringLit
    , BoolLiteral <$> parseBoolLit
    , DecLiteral <$> parseDecLit
    ]

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

parseTupleOrGroup :: Parser Expr
parseTupleOrGroup = do
    void $ symbol "("
    (parseEmptyTuple <|>) do
        e <- parseExpr
        choice [parseTuple [e], parseGroup [e]]
  where
    parseEmptyTuple :: Parser Expr
    parseEmptyTuple = symbol "," *> symbol ")" *> (return $ TupleExpr [])
    parseTuple :: [Expr] -> Parser Expr
    parseTuple t = ((symbol ")") *> (return $ TupleExpr t)) <|> do
        void $ symbol ","
        e <- parseExpr
        parseTuple $ e : t
    parseGroup :: [Expr] -> Parser Expr
    parseGroup t = ((symbol ")") *> (return $ GroupExpr t)) <|> do
        e <- parseExpr
        void $ optional $ symbol ";"
        parseGroup $ e : t


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
        , parseTupleOrGroup
        ]
    optional parseIndexExpr >>= \case
        Just idxVal -> parseNextOp (IndexExpr idxVal e1) prec leftCtor
        Nothing     -> parseNextOp e1 prec leftCtor

parseIdxCallPrec :: Precedence -> (Expr -> Expr) -> Parser Expr
parseIdxCallPrec prec leftCtor = do
    addrExpr <- -- the only callable expr is attribute addressing
                AttrExpr <$> parseAttrAddr
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

