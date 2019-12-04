-- | Event Hosting Interface
module EHI
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
    , insertVar
    , getVar
    )
where

import  qualified         EDH.Decimal as D 
import           EDH.Evaluator.Object as O

