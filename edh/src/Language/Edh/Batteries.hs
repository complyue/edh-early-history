
module Language.Edh.Batteries where

import           Prelude
import           Debug.Trace

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict

import           Data.Ratio
import           Data.IORef
import           Data.Text                     as T
import qualified Data.Map.Strict               as Map

import           Text.Megaparsec

import           Data.Lossless.Decimal         as D

import           Language.Edh.Control
import           Language.Edh.Runtime
import           Language.Edh.Interpreter
import           Language.Edh.Interpreter.Evaluate
import           Language.Edh.AST
import           Language.Edh.Parser


installEdhBatteries :: MonadIO m => EdhWorld -> m ()
installEdhBatteries world = liftIO $ do
    let rootEntity = (objEntity . worldRoot) world

    declareEdhOperators
        world
        "<batteries>"
        [ -- format: (symbol, precedence)

    -- simple assignment
          ( "="
          , 1
          ) -- ^ why brittany insists on formatting it like this ?.?

    -- comprehension
        , ( "<="
          , 3
          ) -- ^ why brittany insists on formatting it like this ?.?

    -- channel read/write
        , ( "<-"
          , 5
          )

    -- basic arithmetic
        , ("+", 6)
        , ("-", 6)
        , ("*", 7)
        , ("/", 7)
        ]

    putEdhAttrs
        rootEntity
        [
    -- builtin operators
    -- TODO these should really be host functions
          (AttrByName "=" , EdhString "the assign op")
        , (AttrByName "<-", EdhString "the cmprh op")
        , (AttrByName "+" , EdhString "plus")
        , (AttrByName "-" , EdhString "minus")
        , (AttrByName "*" , EdhString "mul")
        , ( AttrByName "/"
          , EdhString "div"
          ) -- ^ why brittany insists on formatting it like this ?.?

    -- math constants
    -- todo figure out proper ways to make these really **constant**,
    --      i.e. not rebindable to other values
        , ( AttrByName "pi"
          , EdhDecimal
              $ Decimal 1 (-40) 31415926535897932384626433832795028841971
          )

    -- testing ... 
        , (AttrByName "fff", EdhDecimal $ fromRational $ 3 % 14)
        ]

    return ()

