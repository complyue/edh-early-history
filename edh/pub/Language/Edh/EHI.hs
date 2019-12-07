-- | Event Hosting Interface
module Language.Edh.EHI
    ( Object(..)
    , nil
    , true
    , false
    , nan
    , inf
    , D.Decimal(..)
    , EnvRef
    , emptyEnv
    , wrapEnv
    , setVar
    , getVar
    )
where

import qualified Data.Lossless.Decimal         as D
import           Language.Edh.Runtime.Evaluator.Object
                                               as O

