{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Sonic.Compiler.Desugar.IR.Kind
  ( pattern Type
  , pattern Arrow
  )
where

import           Language.Sonic.Compiler.IR.Tree
                                                ( XWrap )
import           Language.Sonic.Compiler.IR.Kind
                                                ( XType
                                                , XArrow
                                                , XXKind
                                                )
import qualified Language.Sonic.Compiler.IR.Kind
                                               as IR
                                                ( Kind(..) )

import           Language.Sonic.Compiler.Desugar.IR.Pass
                                                ( Desugar )

type instance XType Desugar = ()
type instance XArrow Desugar = ()
type instance XXKind Desugar = ()

type Kind = IR.Kind Desugar

pattern Type :: Kind
pattern Type = IR.Type ()

pattern Arrow :: XWrap Desugar Kind -> XWrap Desugar Kind -> Kind
pattern Arrow l r = IR.Arrow () l r
