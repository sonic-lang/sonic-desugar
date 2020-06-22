{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Sonic.Compiler.Desugar.IR.Type
  ( TypeInfix(..)
  , XTypeDesugar(..)
  , pattern Var
  , pattern Ctor
  , pattern Apply
  , pattern Annotate
  , pattern Forall
  , pattern Parens
  , pattern Infix
  )
where

import           GHC.Generics                   ( Generic )

import           Language.Sonic.Compiler.IR.Tree
                                                ( XRefID
                                                , XWrap
                                                )
import qualified Language.Sonic.Compiler.IR.EntityKind
                                               as Entity
                                                ( TyVar
                                                , TyCtor
                                                )
import           Language.Sonic.Compiler.IR.Kind
                                                ( Kind )
import           Language.Sonic.Compiler.IR.Type
                                                ( XVar
                                                , XCtor
                                                , XApply
                                                , XAnnotate
                                                , XForall
                                                , XXType
                                                )
import qualified Language.Sonic.Compiler.IR.Type
                                               as IR
                                                ( Type(..)
                                                , TyVarBinder
                                                , Context
                                                )

import           Language.Sonic.Compiler.Desugar.IR.Pass
                                                ( Desugar )

type instance XVar Desugar = ()
type instance XCtor Desugar = ()
type instance XApply Desugar = ()
type instance XAnnotate Desugar = ()
type instance XForall Desugar = ()
type instance XXType Desugar = XTypeDesugar

data XTypeDesugar
  = XParens (XWrap Desugar Type)
  | XInfix (XWrap Desugar Type) (XWrap Desugar TypeInfix) (XWrap Desugar Type)
  deriving Generic

newtype TypeInfix
  = TypeInfix (XRefID Entity.TyCtor Desugar)
  deriving Generic

type Type = IR.Type Desugar

pattern Var :: XWrap Desugar (XRefID Entity.TyVar Desugar) -> Type
pattern Var x = IR.Var () x

pattern Ctor :: XWrap Desugar (XRefID Entity.TyCtor Desugar) -> Type
pattern Ctor x = IR.Ctor () x

pattern Apply :: XWrap Desugar Type -> XWrap Desugar Type -> Type
pattern Apply x y = IR.Apply () x y

pattern Annotate :: XWrap Desugar Type -> XWrap Desugar (Kind Desugar) -> Type
pattern Annotate x y = IR.Annotate () x y

pattern Forall :: [XWrap Desugar (IR.TyVarBinder Desugar)] -> Maybe (XWrap Desugar (IR.Context Desugar)) -> XWrap Desugar Type -> Type
pattern Forall v c t = IR.Forall () v c t

pattern Parens :: XWrap Desugar Type -> Type
pattern Parens x = IR.XType (XParens x)

pattern Infix :: XWrap Desugar Type -> XWrap Desugar TypeInfix -> XWrap Desugar Type -> Type
pattern Infix x op y = IR.XType (XInfix x op y)
