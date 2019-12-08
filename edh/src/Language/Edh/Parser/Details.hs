{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Language.Edh.Parser.Details where

import           Prelude

import           Control.Applicative     hiding ( many )
import           Control.Monad
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void
import           Data.Scientific

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

import           Data.Lossless.Decimal

import           Language.Edh.AST

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

optSymbol :: Text -> Parser (Maybe Text)
optSymbol = optional . (L.symbol sc)


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
    ir     <- parseArgsRecv
    impSrc <- parseExpr
    return $ ImportStmt ir impSrc

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
    cr   <- parseArgsRecv
    body <- parseStmt
    return $ ProcDecl cr body

parseArgsRecv :: Parser ArgsReceiver
parseArgsRecv = (symbol "*" >> return WildReceiver) <|> do
    symbol "("
    argRs <- parseArgRecvs [] False
    return $ ArgsReceiver $ reverse argRs
  where
    parseArgRecvs :: [ArgReceiver] -> Bool -> Parser [ArgReceiver]
    parseArgRecvs rs posConsumed = (optional $ symbol ")") >>= \case
        Nothing -> do
            nextArg <- if posConsumed then nextKwArg else nextPosArg
            case nextArg of
                RecvRestArgs _ -> parseArgRecvs (nextArg : rs) True
                _              -> parseArgRecvs (nextArg : rs) False
        _ -> return rs
    nextPosArg, restArgs, nextKwArg :: Parser ArgReceiver
    nextPosArg = restArgs <|> nextKwArg
    restArgs   = do
        symbol "*"
        aname <- parseAttrName
        optSymbol ","
        return $ RecvRestArgs aname
    nextKwArg = do
        aname   <- parseAttrName
        reName  <- optional parseRename
        defExpr <- optional parseDefaultExpr
        optSymbol ","
        return $ RecvArg aname reName defExpr

parseRename :: Parser AttrName
parseRename = do
    symbol "as"
    reName <- parseAttrName
    return reName

parseDefaultExpr :: Parser Expr
parseDefaultExpr = do
    symbol "="
    defExpr <- parseExpr
    return defExpr

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
parseBlockStmt = do
    symbol "{"
    stmts <- many parseStmt
    symbol "}"
    return $ BlockStmt stmts

parseOpDeclOvrdStmt :: Parser Stmt
parseOpDeclOvrdStmt = do
    symbol "("
    opSym <- takeWhile1P (Just "operator symbol") isOperatorChar
    sc
    precDecl <- optional $ L.decimal <* sc
    symbol ")"
    symbol "="
    procDecl <- parseProcDecl
    case precDecl of
        Nothing ->
            -- TODO validate the operator is declared
            OpOvrdStmt opSym <$> parseProcDecl
        Just opPrec ->
            -- TODO validate the operator is not declared yet,
            --      add the op for parse
            return $ OpDeclStmt opSym opPrec procDecl






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
    let withElseClause = do
            symbol "else"
            alt <- parseStmt
            return $ IfExpr cond cseq $ Just alt
    withElseClause <|> (return $ IfExpr cond cseq Nothing)

parseStringLit :: Parser Text
parseStringLit = do
    delim <- char '\"' <|> char '\'' <|> char '`'
    T.pack <$> manyTill L.charLiteral (char delim)

parseBoolLit :: Parser Bool
parseBoolLit =
    (symbol "true" *> return True) <|> (symbol "false" *> return False)

parseDecLit :: Parser Decimal
parseDecLit = try $ lexeme do -- TODO support HEX/OCT ?
    sn <- L.signed (return ()) L.scientific
    return $ Decimal 1 (fromIntegral $ base10Exponent sn) (coefficient sn)

parseLitExpr :: Parser Literal
parseLitExpr = choice
    [ DecLiteral <$> parseDecLit
    , BoolLiteral <$> parseBoolLit
    , StringLiteral <$> parseStringLit
    ]


parseAttrName :: Parser Text
parseAttrName = parseAlphaName <|> parseOpName

parseAlphaName :: Parser Text
parseAlphaName = lexeme do
    anStart <- takeWhile1P Nothing isLetter
    anRest  <- takeWhileP Nothing isIdentChar
    return $ anStart <> anRest

parseOpName :: Parser Text
parseOpName = try do
    symbol "("
    opSym <- takeWhile1P (Just "operator symbol") isOperatorChar
    sc
    symbol ")"
    return $ opSym


parseAttrRef :: Parser AttrRef
parseAttrRef = try $ lexeme do
    an1 <- oneRef
    moreRef [an1] <|> (return $ thisOrOther an1)
  where
    oneRef :: Parser Text
    oneRef = lexeme do
        anStart <- takeWhile1P (Just "legal attribute name") isLetter
        anRest  <- takeWhileP (Just "legal attribute name") isIdentChar
        return $ anStart <> anRest
    moreRef :: [Text] -> Parser AttrRef
    moreRef ans = try do
        void $ symbol "."
        idMore <- oneRef
        moreRef (idMore : ans)
            <|> (return $ RefPath $ map thisOrOther $ reverse ans)
    thisOrOther :: Text -> AttrRef
    thisOrOther = \case
        "this" -> ThisRef
        an     -> NamedRef $ Attribute an


parseExpr :: Parser Expr
parseExpr = choice
    [ parsePrefixExpr
    , parseIfExpr
    , parseListExpr
    , parseDictExpr
    , LitExpr <$> parseLitExpr
    , AttrExpr <$> parseAttrRef
    -- TBD
    ]


parseStmt :: Parser Stmt
parseStmt = choice
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



