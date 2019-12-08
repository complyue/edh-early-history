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


-- | the dict for operator precendences in parsing context
-- no backtracking needed for this, so it can live in the
-- inner monad of 'ParsecT'.
type OpPrecDict = Map.Map OpSymbol (Int, Text)

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
isIdentChar c = isLetter c || isDigit c || elem c ("'$" :: [Char])

isDigit :: Char -> Bool
isDigit = flip elem ['0' .. '9']

isOperatorChar :: Char -> Bool
isOperatorChar = flip elem ("~!@#$%^&|:<>?+-*/" :: [Char])


parseImportStmt :: Parser Stmt
parseImportStmt = do
    void $ symbol "import"
    ir     <- parseArgsReceiver
    impSrc <- parseExpr
    return $ ImportStmt ir impSrc

parseAssignStmt :: Parser Stmt
parseAssignStmt = do
    void $ symbol "let"
    receiver <- parseArgsReceiver
    void $ symbol "="
    sender <- parseArgsSender
    return $ AssignStmt receiver sender

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

parseRetarget :: Parser AttrRef
parseRetarget = do
    void $ symbol "as"
    retgt <- parseAttrRef
    return retgt

parseArgAssignExpr :: Parser Expr
parseArgAssignExpr = do
    void $ symbol "="
    valExpr <- parseExpr
    return valExpr

parseKwRecv :: Bool -> Parser ArgReceiver
parseKwRecv inPack = do
    aref    <- parseAttrRef
    retgt   <- optional parseRetarget
    defExpr <- if inPack then optional parseArgAssignExpr else return Nothing
    case aref of
        ThisRef                 -> fail "can not assign to this"
        SupersRef               -> fail "can not assign to supers"
        DirectRef aname         -> return $ RecvArg aname retgt defExpr
        IndirectRef _expr aname -> do
            case retgt of
                Nothing -> return $ RecvArg aname (Just aref) defExpr
                Just tgt ->
                    fail
                        $  "can not retarget "
                        <> show aref
                        <> " to "
                        <> show tgt

parseSupersRef :: Parser AttrRef
parseSupersRef = SupersRef <$ symbol "supers"

parseAttrRef :: Parser AttrRef
parseAttrRef = parseSupersRef <|> try do
    p1 <- firstPart
    nextRef p1 <|> case p1 of
        AttrExpr r1 -> return r1
        _expr       -> error "bug"
  where
    firstPart :: Parser Expr
    firstPart = choice
        [ (AttrExpr ThisRef) <$ symbol "this"
        , (AttrExpr . DirectRef) <$> parseAttrName
        ]
    nextRef :: Expr -> Parser AttrRef
    nextRef p1 = do
        void $ symbol "."
        aname <- parseAttrName
        return $ IndirectRef p1 aname


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
        optional parseArgAssignExpr >>= \case
            Nothing      -> return $ SendPosArg p1
            Just valExpr -> case p1 of
                AttrExpr aref -> case aref of
                    DirectRef aname -> return $ SendKwArg aname valExpr
                    _ -> fail $ "invalid argument name: " <> show aref
                _ -> fail $ "invalid argument name: " <> show valExpr


parseClassStmt :: Parser Stmt
parseClassStmt = do
    void $ symbol "class"
    cname    <- parseAttrName
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
    mname    <- parseAttrName
    procDecl <- parseProcDecl
    return $ MethodStmt mname procDecl

parseForStmt :: Parser Stmt
parseForStmt = do
    void $ symbol "for"
    ar   <- parseArgsReceiver
    coll <- parseExpr
    stmt <- parseStmt
    return $ ForStmt ar coll stmt

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

parseBlockStmt :: Parser Stmt
parseBlockStmt =
    BlockStmt <$> (between (symbol "{") (symbol "}") $ many parseStmt)

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
                , parseForStmt
                  -- TODO validate break/continue must within for block
                , BreakStmt <$ symbol "break"
                , ContinueStmt <$ symbol "continue"
                , parseOpDeclOvrdStmt
                , parseTryStmt
                , parseBlockStmt
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
    cseq <- parseStmt
    alt  <- optional do
        void $ symbol "else"
        parseStmt
    return $ IfExpr cond cseq alt

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

parseParenExpr :: Parser Expr
parseParenExpr = between (symbol "(") (symbol ")") parseExpr

parseAttrName :: Parser Text
parseAttrName = parseOpName <|> parseAlphaName

parseAlphaName :: Parser Text
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


parseExpr :: Parser Expr
parseExpr = parseExprPrec 0 id


parseExprPrec :: Precedence -> (Expr -> Expr) -> Parser Expr
parseExprPrec prec leftCtor = choice
    [ parsNonIdxNonCallPrec prec leftCtor
    , parseIdxNonCallPrec prec leftCtor
    , parseIdxCallPrec prec leftCtor
    ]

parsNonIdxNonCallPrec :: Precedence -> (Expr -> Expr) -> Parser Expr
parsNonIdxNonCallPrec prec leftCtor = do
    e1 <- choice [parseIfExpr, parsePrefixExpr, LitExpr <$> parseLitExpr]
    parseNextOp e1 prec leftCtor

parseIdxNonCallPrec :: Precedence -> (Expr -> Expr) -> Parser Expr
parseIdxNonCallPrec prec leftCtor = do
    e1 <- choice [AttrExpr <$> parseSupersRef, parseListExpr, parseDictExpr]
    optional parseIndexExpr >>= \case
        Just idxVal -> parseNextOp (IndexExpr idxVal e1) prec leftCtor
        Nothing     -> parseNextOp e1 prec leftCtor

parseIdxCallPrec :: Precedence -> (Expr -> Expr) -> Parser Expr
parseIdxCallPrec prec leftCtor = do
    e1 <- choice [parseParenExpr, AttrExpr <$> parseAttrRef]
    optional parseIndexExpr >>= \case
        Just idxVal -> parseNextOp (IndexExpr idxVal e1) prec leftCtor
        Nothing     -> optional parsePackSender >>= \case
            Just packSender ->
                parseNextOp (CallExpr e1 packSender) prec leftCtor
            Nothing -> parseNextOp e1 prec leftCtor

parseNextOp :: Expr -> Precedence -> (Expr -> Expr) -> Parser Expr
parseNextOp e1 prec leftCtor = do
    optional parseOpLit >>= \case
        Nothing    -> return e1
        Just opSym -> do
            opPD <- get
            case Map.lookup opSym opPD of
                Nothing -> fail $ "undeclared operator: " <> T.unpack opSym
                Just (opPrec, _opDeclPos) ->
                    parseExprPrec opPrec $ \nextExpr -> if prec < opPrec
                        then leftCtor $ InfixExpr opSym e1 nextExpr
                        else InfixExpr opSym (leftCtor e1) nextExpr

