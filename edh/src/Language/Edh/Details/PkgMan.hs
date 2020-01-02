
module Language.Edh.Details.PkgMan where

import           Prelude
-- import           Debug.Trace

import           GHC.Conc                       ( unsafeIOToSTM )

import           System.IO
import           System.FilePath
import           System.Directory

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Concurrent.STM

import qualified Data.Text                     as T
import qualified Data.Map.Strict               as Map
import           Data.List
import           Data.List.NonEmpty             ( NonEmpty(..)
                                                , (<|)
                                                )
import qualified Data.List.NonEmpty            as NE

import           Text.Megaparsec

import           Language.Edh.Control
import           Language.Edh.AST
import           Language.Edh.Details.RtTypes
import           Language.Edh.Details.Utils


-- | returns nominal path and actual path, the two will be different
-- e.g. in case an `index.edh` for a package, where nominal path will 
-- point to the root directory
locateEdhModule
  :: EdhProgState -> FilePath -> FilePath -> STM (FilePath, FilePath)
locateEdhModule !pgs !fromPath !importPath =
  (unsafeIOToSTM $ doesPathExist fromPath) >>= \case
    False ->
      throwEdhFromSTM pgs EvalError
        $  "Path does not exists: "
        <> T.pack importPath
    True -> case stripPrefix "./" importPath of
      Just !relImp -> resolveRelImport relImp
      Nothing      -> resolveAbsImport
 where
  resolveRelImport !relImp = do
    let !nomPath = fromPath </> relImp
    return (nomPath, nomPath)

  resolveAbsImport = undefined

