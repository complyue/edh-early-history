module Evaluator.BuiltIns where

import           RIO

import           Prelude                        ( putStrLn )
import qualified Data.Text                     as T

import           Decimal
import           Evaluator.Object
import           Parser.AST                     ( Ident(Ident) )

breturn :: Object -> IO BuiltInFnResult
breturn = return . Right

bthrow :: Text -> IO BuiltInFnResult
bthrow = return . Left

invalid :: Object -> Text -> IO BuiltInFnResult
invalid f argText =
    bthrow $ "invalid arguments for " <> tshow f <> ": " <> argText

len :: Object
len = OBuiltInFn "len" 1 go
  where
    go :: BuiltInFn
    go [OString t] =
        breturn . ODecimal . (Decimal 0 1) . fromIntegral $ T.length t
    go [OArray arr] =
        breturn . ODecimal . (Decimal 0 1) . fromIntegral $ length arr
    go args = invalid len $ tshow args

bhead :: Object
bhead = OBuiltInFn "head" 1 go
  where
    go :: BuiltInFn
    go [OArray []     ] = invalid bhead "empty array"
    go [OArray (x : _)] = breturn x
    go args             = invalid bhead $ tshow args

btail :: Object
btail = OBuiltInFn "tail" 1 go
  where
    go :: BuiltInFn
    go [OArray []      ] = invalid btail "empty array"
    go [OArray (_ : xs)] = breturn $ OArray xs
    go args              = invalid btail $ tshow args

bcons :: Object
bcons = OBuiltInFn "cons" 2 go
  where
    go :: BuiltInFn
    go [o, OArray os] = breturn . OArray $ o : os
    go args           = invalid bcons $ tshow args

bprint :: Object
bprint = OBuiltInFn "print" 1 go
  where
    go :: BuiltInFn
    go [OString t] = putStrLn (T.unpack t) >> breturn nil
    go [o        ] = putStrLn (show o) >> breturn nil
    go args        = invalid bprint $ tshow args

builtIns :: [(Ident, Object)]
builtIns =
    [ (Ident "null" , nil)
    , (Ident "nan"  , nan)
    , (Ident "inf"  , inf)
    , (Ident "len"  , len)
    , (Ident "head" , bhead)
    , (Ident "tail" , btail)
    , (Ident "cons" , bcons)
    , (Ident "print", bprint)
    ]
