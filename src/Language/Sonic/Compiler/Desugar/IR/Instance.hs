{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# OPTIONS_GHC -Wno-orphans    #-}

module Language.Sonic.Compiler.Desugar.IR.Instance
  ()
where

import           Language.Sonic.Compiler.Desugar.IR.Expression
import           Language.Sonic.Compiler.Desugar.IR.Kind
                                                ( )
import           Language.Sonic.Compiler.Desugar.IR.Module
                                                ( )
import           Language.Sonic.Compiler.Desugar.IR.Pattern
import           Language.Sonic.Compiler.Desugar.IR.Type

import qualified Language.Sonic.Compiler.IR.Pattern
                                               as IR
import qualified Language.Sonic.Compiler.IR.Kind
                                               as IR
import qualified Language.Sonic.Compiler.IR.Type
                                               as IR
import qualified Language.Sonic.Compiler.IR.Expression
                                               as IR
import qualified Language.Sonic.Compiler.IR.Attribute
                                               as IR
import qualified Language.Sonic.Compiler.IR.Declaration
                                               as IR
import qualified Language.Sonic.Compiler.IR.Module
                                               as IR

import           Language.Sonic.Compiler.Desugar.IR.Pass
                                                ( Desugar )

deriving instance Show (IR.Module Desugar)
deriving instance Eq (IR.Module Desugar)

deriving instance Show (IR.Attrs Desugar)
deriving instance Show (IR.Attr Desugar)
deriving instance Show (IR.AttrValue Desugar)
deriving instance Show (IR.EntityRef Desugar)
deriving instance Eq (IR.Attrs Desugar)
deriving instance Eq (IR.Attr Desugar)
deriving instance Eq (IR.AttrValue Desugar)
deriving instance Eq (IR.EntityRef Desugar)

deriving instance Show (IR.BindGroup Desugar)
deriving instance Show (IR.Bind Desugar)
deriving instance Eq (IR.BindGroup Desugar)
deriving instance Eq (IR.Bind Desugar)

deriving instance Show (IR.Type Desugar)
deriving instance Show XTypeDesugar
deriving instance Show TypeInfix
deriving instance Show (IR.Kind Desugar)
deriving instance Show (IR.TyVarBinder Desugar)
deriving instance Show (IR.Context Desugar)
deriving instance Show (IR.Predicate Desugar)
deriving instance Eq (IR.Type Desugar)
deriving instance Eq XTypeDesugar
deriving instance Eq TypeInfix
deriving instance Eq (IR.Kind Desugar)
deriving instance Eq (IR.TyVarBinder Desugar)
deriving instance Eq (IR.Context Desugar)
deriving instance Eq (IR.Predicate Desugar)

deriving instance Show (IR.Expr Desugar)
deriving instance Show XExprDesugar
deriving instance Show ExprInfix
deriving instance Show (IR.CaseArm Desugar)
deriving instance Eq (IR.Expr Desugar)
deriving instance Eq XExprDesugar
deriving instance Eq ExprInfix
deriving instance Eq (IR.CaseArm Desugar)

deriving instance Show (IR.Pat Desugar)
deriving instance Show XPatDesugar
deriving instance Show PatInfix
deriving instance Eq (IR.Pat Desugar)
deriving instance Eq XPatDesugar
deriving instance Eq PatInfix

deriving instance Show (IR.DataDecl Desugar)
deriving instance Show (IR.DataCtorDecl Desugar)
deriving instance Eq (IR.DataDecl Desugar)
deriving instance Eq (IR.DataCtorDecl Desugar)

deriving instance Show (IR.ClassDecl Desugar)
deriving instance Show (IR.ClassMethodDecl Desugar)
deriving instance Eq (IR.ClassDecl Desugar)
deriving instance Eq (IR.ClassMethodDecl Desugar)

deriving instance Show (IR.InstanceDecl Desugar)
deriving instance Eq (IR.InstanceDecl Desugar)
