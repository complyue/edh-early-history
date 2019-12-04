module EDH.Parser where

import           RIO                     hiding ( fail
                                                , many
                                                , optional
                                                )

import           EDH.Common.ParserT
import qualified EDH.Lexer.Token               as Tk
import           EDH.Parser.AST
import           EDH.Parser.Types
import           EDH.Utils                      ( (<<) )

parseProgram :: Parser Program
parseProgram = Program <$> many parseStmt

parseStmt :: Parser Stmt
parseStmt = choose [parseLetStmt, parseReturnStmt, parseExprStmt]

parseIdent :: Parser Ident
parseIdent = next >>= go
  where
    go (Tk.Ident name) = return $ Ident name
    go _               = fail "fail to parse an identifier"

parseLetStmt :: Parser Stmt
parseLetStmt = do
    ident <- parseIdent
    void $ atom Tk.Assign
    expr <- parseExpr
    optional $ atom Tk.SemiColon
    return $ AssignStmt ident expr

parseReturnStmt :: Parser Stmt
parseReturnStmt = do
    void $ atom Tk.Return
    expr <- parseExpr
    optional $ atom Tk.SemiColon
    return $ ReturnStmt expr

parseExprStmt :: Parser Stmt
parseExprStmt = ExprStmt <$> do
    expr <- parseExpr
    optional $ atom Tk.SemiColon
    return expr

parseBlockStmt :: Parser BlockStmt
parseBlockStmt = do
    void $ atom Tk.LBrace
    ss <- many parseStmt
    void $ atom Tk.RBrace
    return ss

infixOp :: Tk.Token -> (Precedence, Maybe Infix)
infixOp Tk.Eq          = (PEquals, Just Eq)
infixOp Tk.NotEq       = (PEquals, Just NotEq)
infixOp Tk.LessThan    = (PLessGreater, Just LessThan)
infixOp Tk.GreaterThan = (PLessGreater, Just GreaterThan)
infixOp Tk.Plus        = (PSum, Just Plus)
infixOp Tk.Minus       = (PSum, Just Minus)
infixOp Tk.Multiply    = (PProduct, Just Multiply)
infixOp Tk.Divide      = (PProduct, Just Divide)
infixOp Tk.LParen      = (PCall, Nothing) -- for call expr
infixOp Tk.LBracket    = (PIndex, Nothing) -- for index expr
infixOp _              = (PLowest, Nothing)

parseAtomExpr :: Parser Expr
parseAtomExpr = choose
    [ parseLitExpr
    , parseIdentExpr
    , parsePrefixExpr
    , parseParenExpr
    , parseArrayExpr
    , parseHashExpr
    , parseIfExpr
    , parseFnExpr
    ]

parseParenExpr :: Parser Expr
parseParenExpr = do
    void $ atom Tk.LParen
    expr <- parseExpr
    void $ atom Tk.RParen
    return expr

parseArrayExpr :: Parser Expr
parseArrayExpr = do
    void $ atom Tk.LBracket
    exprs <- parseExprs <|> return []
    void $ atom Tk.RBracket
    return $ ArrayExpr exprs
  where
    parseExprs :: Parser [Expr]
    parseExprs = do
        e  <- parseExpr
        es <- many $ do
            void $ atom Tk.Comma
            parseExpr
        return $ e : es

parseHashExpr :: Parser Expr
parseHashExpr = do
    void $ atom Tk.LBrace
    pairs <-
        (parseHashPair >>= \pair -> do
                morePairs <- many $ atom Tk.Comma >> parseHashPair
                return $ pair : morePairs
            )
            <|> return []
    void $ atom Tk.RBrace
    return $ HashExpr pairs
  where
    parseHashPair :: Parser (Literal, Expr)
    parseHashPair = do
        l <- parseLiteral
        void $ atom Tk.Colon
        e <- parseExpr
        return $ (l, e)

parseLiteral :: Parser Literal
parseLiteral = next >>= go
  where
    go (Tk.DecLiteral n e ) = return $ DecLiteral n e
    go (Tk.BoolLiteral   b) = return $ BoolLiteral b
    go (Tk.StringLiteral s) = return $ StringLiteral s
    go _                    = fail "fail to parse a literal"

parseExpr :: Parser Expr
parseExpr = parsePrattExpr PLowest

parsePrattExpr :: Precedence -> Parser Expr
parsePrattExpr precedence = do
    left <- parseAtomExpr
    go precedence left
  where
    go :: Precedence -> Expr -> Parser Expr
    go p left = do
        maybePeekInfixOp <- fmap infixOp <$> preview
        case maybePeekInfixOp of
            Just (PCall, _) | p < PCall -> do
                left' <- parseCallExpr left
                go p left'
            Just (PIndex, _) | p < PIndex -> do
                left' <- parseIndexExpr left
                go p left'
            Just (peekPrecedence, _) | p < peekPrecedence -> do
                left' <- parseInfixExpr left
                go p left'
            _ -> return left

parsePrefixExpr :: Parser Expr
parsePrefixExpr = do
    tkn <- choose [atom Tk.Plus, atom Tk.Minus, atom Tk.Not]
    case tkn of
        Tk.Plus  -> PrefixExpr PrefixPlus <$> parseAtomExpr
        Tk.Minus -> PrefixExpr PrefixMinus <$> parseAtomExpr
        Tk.Not   -> PrefixExpr Not <$> parseAtomExpr
        _        -> fail "fail to parse a prefix expr"

parseInfixExpr :: Expr -> Parser Expr
parseInfixExpr left = do
    (precedence, maybeOp) <- infixOp <$> next
    case maybeOp of
        Nothing -> fail "not infix expr"
        Just op -> do
            right <- parsePrattExpr precedence
            return $ InfixExpr op left right

parseCallExpr :: Expr -> Parser Expr
parseCallExpr fnHandle = do
    void $ atom Tk.LParen
    args <-
        (parseExpr >>= \arg -> do
                moreArgs <- many $ atom Tk.Comma >> parseExpr
                return $ arg : moreArgs
            )
            <|> return []
    void $ atom Tk.RParen
    return $ CallExpr fnHandle args

parseIndexExpr :: Expr -> Parser Expr
parseIndexExpr arr = do
    void $ atom Tk.LBracket
    idx <- parseExpr
    void $ atom Tk.RBracket
    return $ IndexExpr arr idx

parseLitExpr :: Parser Expr
parseLitExpr = LitExpr <$> parseLiteral

parseIdentExpr :: Parser Expr
parseIdentExpr = IdentExpr <$> parseIdent

parseIfExpr :: Parser Expr
parseIfExpr = do
    void $ atom Tk.If
    void $ atom Tk.LParen
    expr <- parseExpr
    void $ atom Tk.RParen
    consequence_ <- parseBlockStmt
    IfExpr expr consequence_
        <$> (   (do
                    void $ atom Tk.Else
                    Just <$> parseBlockStmt
                )
            <|> return Nothing
            )

parseFnExpr :: Parser Expr
parseFnExpr = do
    void $ atom Tk.Function
    void $ atom Tk.LParen
    params_ <- parseParams <|> return []
    void $ atom Tk.RParen
    FnExpr params_ <$> parseBlockStmt
  where
    parseParams :: Parser [Ident]
    parseParams = do
        p  <- parseIdent
        ps <- many $ do
            void $ atom Tk.Comma
            parseIdent
        return $ p : ps

finish :: Parser ()
finish = next >>= go
  where
    go Tk.EOF = return ()
    go tkn    = fail $ "unexpected token: " ++ show tkn

parse :: [Tk.Token] -> Either ParserError Program
parse = execParser (parseProgram << finish)
