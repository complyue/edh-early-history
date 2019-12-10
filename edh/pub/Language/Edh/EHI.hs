-- | Edh Host Interface
--
-- With Haskell as the host language, Edh as the guest language,
-- this defines the interface for Edh hosting programs to create
-- & control embed Edh worlds, and to splice host (typically
-- side-effects free, i.e. pure, and performant) functions in
-- Haskell, with object-orient, while highly mutable (thus
-- world-changing), but not-so-performant (as being interpreted)
-- procedures in Edh.
module Language.Edh.EHI
    ( EdhValue(..)
    , nil
    , true
    , false
    , nan
    , inf
    , D.Decimal(..)
    )
where

import qualified Data.Lossless.Decimal         as D

import           Language.Edh.Runtime
import           Language.Edh.Batteries

