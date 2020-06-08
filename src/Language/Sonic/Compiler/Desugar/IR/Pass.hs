{-# LANGUAGE TypeFamilies #-}

module Language.Sonic.Compiler.Desugar.IR.Pass
  ( Desugar
  , passDesugar
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Proxy                     ( Proxy(..) )

import           Language.Sonic.Compiler.IR.Tree
                                                ( XRefID
                                                , XDefID
                                                , XWrap
                                                )
import           Language.Sonic.Compiler.Pass   ( Pass
                                                , PassIndex(..)
                                                , pass
                                                )
import           Language.Sonic.Compiler.Path   ( Path
                                                , Name
                                                )
import           Language.Sonic.Compiler.Provenance
                                                ( WithProv )

data Desugar
  deriving Generic

instance PassIndex Desugar where
  name _ = "desugar"

passDesugar :: Pass
passDesugar = pass (Proxy :: Proxy Desugar)

type instance XRefID k Desugar = Path k
type instance XDefID k Desugar = Name k

type instance XWrap Desugar = WithProv
