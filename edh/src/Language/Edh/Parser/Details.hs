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
import           Control.Monad.State.Class
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
    symbol "import"
    ir     <- parseArgsReceiver
    impSrc <- parseExpr
    return $ ImportStmt ir impSrc

parseAssignStmt :: Parser Stmt
parseAssignStmt = do
    symbol "let"
    receiver <- parseArgsReceiver
    symbol "="
    sender <- parseArgsSender
    return $ AssignStmt receiver sender

parseArgsReceiver :: Parser ArgsReceiver
parseArgsReceiver =
    (symbol "*" >> return WildReceiver) <|> parsePackReceiver <|> do
        singleArg <- parseKwRecv False
        return $ SingleReceiver singleArg

parsePackReceiver :: Parser ArgsReceiver
parsePackReceiver = between
    (symbol "(")
    (symbol ")")
    do
        argRs <- parseArgRecvs [] False
        return $ PackReceiver $ reverse argRs

parseArgRecvs :: [ArgReceiver] -> Bool -> Parser [ArgReceiver]
parseArgRecvs rs posConsumed = (lookAhead (symbol ")") >> return rs) <|> do
    nextArg <-
        (if posConsumed then parseKwRecv True else nextPosArg) <* trailingComma
    case nextArg of
        RecvRestArgs _ -> parseArgRecvs (nextArg : rs) True
        _              -> parseArgRecvs (nextArg : rs) False
  where
    nextPosArg, restArgs :: Parser ArgReceiver
    nextPosArg = restArgs <|> parseKwRecv True
    restArgs   = do
        symbol "*"
        aname <- parseAttrName
        return $ RecvRestArgs aname

parseRetarget :: Parser AttrRef
parseRetarget = do
    symbol "as"
    retgt <- parseAttrRef
    return retgt

parseArgAssignExpr :: Parser Expr
parseArgAssignExpr = do
    symbol "="
    valExpr <- parseExpr
    return valExpr

parseKwRecv :: Bool -> Parser ArgReceiver
parseKwRecv inPack = do
    aref    <- parseAttrRef
    retgt   <- optional parseRetarget
    defExpr <- if inPack then optional parseArgAssignExpr else return Nothing
    case aref of
        ThisRef                -> fail "can not assign to this"
        SupersRef              -> fail "can not assign to supers"
        DirectRef aname        -> return $ RecvArg aname retgt defExpr
        IndirectRef expr aname -> do
            case retgt of
                Nothing -> return $ RecvArg aname (Just aref) defExpr
                Just tgt ->
                    fail
                        $  "can not retarget "
                        <> show aref
                        <> " to "
                        <> show tgt

parseAttrRef :: Parser AttrRef
parseAttrRef = do
    p1 <- firstPart
    nextRef p1 <|> case p1 of
        AttrExpr r1 -> return r1
        expr        -> fail $ "invalid attribute reference: " <> show expr
  where
    firstPart :: Parser Expr
    firstPart = choice
        [ (AttrExpr ThisRef) <$ symbol "this"
        , (AttrExpr SupersRef) <$ symbol "supers"
        , parseParenExpr
        , (AttrExpr . DirectRef) <$> parseAttrName
        ]
    nextRef :: Expr -> Parser AttrRef
    nextRef p1 = do
        symbol "."
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
        symbol "**"
        expr <- parseExpr
        return $ UnpackKwArgs expr
    unpackPosArgs = do
        symbol "*"
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
    symbol "class"
    cname    <- parseAttrName
    procDecl <- parseProcDecl
    return $ ClassStmt cname procDecl

parseExtendsStmt :: Parser Stmt
parseExtendsStmt = do
    symbol "extends"
    superExpr <- parseExpr
    return $ ExtendsStmt superExpr

parseMethodStmt :: Parser Stmt
parseMethodStmt = do
    symbol "method"
    mname    <- parseAttrName
    procDecl <- parseProcDecl
    return $ MethodStmt mname procDecl

parseProcDecl :: Parser ProcDecl
parseProcDecl = do
    cr   <- parseArgsReceiver
    body <- parseStmt
    return $ ProcDecl cr body

parseOpDeclOvrdStmt :: Parser Stmt
parseOpDeclOvrdStmt = do
    symbol "operator"
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

parseReturnStmt :: Parser Stmt
parseReturnStmt = do
    symbol "return"
    expr <- parseExpr
    return $ ReturnStmt expr

parseTryStmt :: Parser Stmt
parseTryStmt = do
    symbol "try"
    trunk <- parseStmt
    let withOneCatch = try do
            symbol "catch"
            excClass <- parseExpr
            an       <-
                withRecovery (const $ return Nothing)
                $   Just
                <$> (symbol "as" >> parseAttrName)
            recov <- parseStmt
            return $ (excClass, an, recov)
    catches <- many withOneCatch
    let withFinallyStmt = do
            symbol "finally"
            final <- parseStmt
            return $ TryStmt trunk catches $ Just final
    withFinallyStmt <|> (return $ TryStmt trunk catches Nothing)

parseBlockStmt :: Parser Stmt
parseBlockStmt =
    BlockStmt <$> (between (symbol "{") (symbol "}") $ many parseStmt)


parseStmt :: Parser Stmt
parseStmt =
    choice
            [ parseImportStmt
            , parseClassStmt
            , parseExtendsStmt
            , parseMethodStmt
            , parseReturnStmt
            , parseTryStmt
            , parseBlockStmt
            , parseOpDeclOvrdStmt
            , ExprStmt <$> parseExpr
            ]
        <* trailingColon


parsePrefixExpr :: Parser Expr
parsePrefixExpr = choice
    [ PrefixExpr PrefixPlus <$> (symbol "+" >> parseExpr)
    , PrefixExpr PrefixMinus <$> (symbol "-" >> parseExpr)
    , PrefixExpr Not <$> (symbol "not" >> parseExpr)
    ]

parseIfExpr :: Parser Expr
parseIfExpr = do
    symbol "if"
    cond <- parseExpr
    symbol "then"
    cseq <- parseStmt
    alt  <- optional do
        symbol "else"
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
        symbol ":"
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
parseAttrName = parseAlphaName <|> parseOpName

parseAlphaName :: Parser Text
parseAlphaName = lexeme do
    anStart <- takeWhile1P (Just "attribute name") isLetter
    anRest  <- takeWhileP Nothing isIdentChar
    return $ anStart <> anRest

parseOpName :: Parser Text
parseOpName = between (symbol "(") (symbol ")") parseOpLit

parseOpLit :: Parser Text
parseOpLit = lexeme $ takeWhile1P (Just "operator symbol") isOperatorChar


parseExpr :: Parser Expr
parseExpr = choice
    [ parsePrefixExpr
    , parseIfExpr
    , parseListExpr
    , parseDictExpr
    , parseParenExpr
    , parseExprPrec
    ]


parseExprPrec :: Parser Expr
parseExprPrec = undefined

