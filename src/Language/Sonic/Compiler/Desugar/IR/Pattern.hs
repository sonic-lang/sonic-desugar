{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Sonic.Compiler.Desugar.IR.Pattern
  ( PatInfix(..)
  , pattern Wildcard
  , pattern Literal
  , pattern Var
  , pattern Ctor
  , pattern Parens
  , pattern Infix
  )
where

import           GHC.Generics                   ( Generic )

import           Language.Sonic.Compiler.IR.Tree
                                                ( XRefID
                                                , XDefID
                                                , XWrap
                                                )
import qualified Language.Sonic.Compiler.IR.EntityKind
                                               as Entity
                                                ( Var
                                                , Ctor
                                                )
import           Language.Sonic.Compiler.IR.Literal
                                                ( Literal )
import           Language.Sonic.Compiler.IR.Pattern
                                                ( XWildcard
                                                , XLiteral
                                                , XVar
                                                , XCtor
                                                , XXPat
                                                )
import qualified Language.Sonic.Compiler.IR.Pattern
                                               as IR
                                                ( Pat(..) )

import           Language.Sonic.Compiler.Desugar.IR.Pass
                                                ( Desugar )

type instance XWildcard Desugar = ()
type instance XLiteral Desugar = ()
type instance XVar Desugar = ()
type instance XCtor Desugar = ()
type instance XXPat Desugar = XPatDesugar

data XPatDesugar
  = XParens (XWrap Desugar Pat)
  | XInfix (XWrap Desugar Pat) (XWrap Desugar PatInfix) (XWrap Desugar Pat)
  deriving Generic

newtype PatInfix = PatInfix (XRefID Entity.Ctor Desugar)
  deriving Generic

type Pat = IR.Pat Desugar

pattern Wildcard :: Pat
pattern Wildcard = IR.Wildcard ()

pattern Literal :: XWrap Desugar Literal -> Pat
pattern Literal x = IR.Literal () x

pattern Var :: XWrap Desugar (XDefID Entity.Var Desugar) -> Pat
pattern Var x = IR.Var () x

pattern Ctor :: XWrap Desugar (XRefID Entity.Ctor Desugar) -> [XWrap Desugar Pat] -> Pat
pattern Ctor x y = IR.Ctor () x y

pattern Parens :: XWrap Desugar Pat -> Pat
pattern Parens x = IR.XPat (XParens x)

pattern Infix :: XWrap Desugar Pat -> XWrap Desugar PatInfix -> XWrap Desugar Pat -> Pat
pattern Infix x op y = IR.XPat (XInfix x op y)
