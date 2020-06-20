{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Language.Sonic.Compiler.Desugar.Desugarable
  ( Desugarable(..)
  )
where

import qualified Language.Sonic.Parser         as Syn
                                                ( Position )
import           Language.Sonic.Compiler.Context
                                                ( FileContext )
import           Language.Sonic.Compiler.Unique ( MonadUnique )
import           Language.Sonic.Compiler.Report ( MonadReport )
import           Language.Sonic.Compiler.Provenance
                                                ( WithProv )
import           Language.Sonic.Compiler.Desugar.Internal
                                                ( generated )

import           Language.Sonic.Compiler.Path   ( Name
                                                , Path
                                                , PathPrefix
                                                )
import           Language.Sonic.Compiler.IR.EntityKind
                                                ( Var
                                                , Ctor
                                                , TyVar
                                                , TyCtor
                                                , Module
                                                )

import qualified Language.Sonic.Syntax.Name    as Syn
import qualified Language.Sonic.Syntax.Path    as Syn
import qualified Language.Sonic.Syntax.Literal as Syn
import qualified Language.Sonic.Syntax.Pattern as Syn
import qualified Language.Sonic.Syntax.Kind    as Syn
import qualified Language.Sonic.Syntax.Type    as Syn
import qualified Language.Sonic.Syntax.Expression
                                               as Syn
import qualified Language.Sonic.Syntax.Attribute
                                               as Syn
import qualified Language.Sonic.Syntax.Declaration
                                               as Syn
import qualified Language.Sonic.Syntax.Module  as Syn

import qualified Language.Sonic.Compiler.IR.Literal
                                               as IR
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

import qualified Language.Sonic.Compiler.Desugar.IR.Pattern
                                               as IR
import qualified Language.Sonic.Compiler.Desugar.IR.Type
                                               as IR
import qualified Language.Sonic.Compiler.Desugar.IR.Expression
                                               as IR

import           Language.Sonic.Compiler.Desugar.Name
import           Language.Sonic.Compiler.Desugar.Path
import           Language.Sonic.Compiler.Desugar.Literal
import           Language.Sonic.Compiler.Desugar.Pattern
import           Language.Sonic.Compiler.Desugar.Kind
import           Language.Sonic.Compiler.Desugar.Type
import           Language.Sonic.Compiler.Desugar.Expression
import           Language.Sonic.Compiler.Desugar.Attribute
import           Language.Sonic.Compiler.Desugar.Declaration
import           Language.Sonic.Compiler.Desugar.Module

import           Language.Sonic.Compiler.Desugar.IR.Pass
                                                ( Desugar )

class Desugarable syn ir | syn -> ir where
  desugar :: (FileContext m, MonadUnique m, MonadReport m) => syn Syn.Position -> m ir

-- Name

instance Desugarable Syn.CtorName (Name Ctor) where
  desugar = pure . desugarCtorName

instance Desugarable Syn.VarName (Name Var) where
  desugar = pure . desugarVarName

instance Desugarable Syn.TyVarName (Name TyVar) where
  desugar = pure . desugarTyVarName

instance Desugarable Syn.TyCtorName (Name TyCtor) where
  desugar = pure . desugarTyCtorName

instance Desugarable Syn.ModuleComponentName (Name Module) where
  desugar = pure . desugarModuleComponentName

instance Desugarable Syn.AttrKeyName IR.AttrKey where
  desugar = pure . desugarAttrKeyName

-- Path

instance Desugarable Syn.PathPrefix PathPrefix where
  desugar = pure . desugarPathPrefix

instance Desugarable (Syn.Path Syn.CtorName) (Path Ctor) where
  desugar = pure . desugarPath desugarCtorName

instance Desugarable (Syn.Path Syn.VarName) (Path Var) where
  desugar = pure . desugarPath desugarVarName

instance Desugarable (Syn.Path Syn.TyVarName) (Path TyVar) where
  desugar = pure . desugarPath desugarTyVarName

instance Desugarable (Syn.Path Syn.TyCtorName) (Path TyCtor) where
  desugar = pure . desugarPath desugarTyCtorName

-- Literal

instance Desugarable Syn.Literal IR.Literal where
  desugar = pure . desugarLiteral

-- Pattern

instance Desugarable Syn.Pat (IR.Pat Desugar) where
  desugar = desugarPat

instance Desugarable Syn.PatInfix IR.PatInfix where
  desugar = pure . desugarPatInfix

-- Kind

instance Desugarable Syn.Kind (IR.Kind Desugar) where
  desugar = desugarKind

-- Type

instance Desugarable Syn.Type (IR.Type Desugar) where
  desugar = desugarType

instance Desugarable Syn.TypeInfix IR.TypeInfix where
  desugar = pure . desugarTypeInfix

instance Desugarable Syn.TyVarBinder (IR.TyVarBinder Desugar) where
  desugar = desugarTyVarBinder

instance Desugarable Syn.Context (IR.Context Desugar) where
  desugar = desugarContext

instance Desugarable Syn.Predicate (IR.Predicate Desugar) where
  desugar = desugarPredicate

-- Expression

instance Desugarable Syn.Expr (IR.Expr Desugar) where
  desugar = desugarExpr

instance Desugarable Syn.ExprInfix IR.ExprInfix where
  desugar = pure . desugarExprInfix

instance Desugarable Syn.LetDefn [IR.Bind Desugar] where
  desugar = desugarLetDefn

instance Desugarable Syn.CaseArm (IR.CaseArm Desugar) where
  desugar = desugarCaseArm

instance Desugarable Syn.Guard (IR.Expr Desugar) where
  desugar = desugarGuard

-- Attribute

instance Desugarable Syn.AttrSet (IR.Attrs Desugar) where
  desugar = desugarAttrSet

instance Desugarable Syn.AttrValue (IR.AttrValue Desugar) where
  desugar = pure . desugarAttrValue

instance Desugarable Syn.AttrValueList [WithProv (IR.AttrValue Desugar)] where
  desugar = desugarAttrValueList

-- Declaration

instance Desugarable Syn.DataDecl (IR.DataDecl Desugar) where
  -- | Note that attributes are left empty
  desugar = desugarDataDecl (generated mempty)

instance Desugarable Syn.ClassDecl (IR.ClassDecl Desugar) where
  -- | Note that attributes are left empty
  desugar = desugarClassDecl (generated mempty)

instance Desugarable Syn.InstanceDecl (IR.InstanceDecl Desugar) where
  -- | Note that attributes are left empty
  desugar = desugarInstanceDecl (generated mempty)

-- Module

instance Desugarable Syn.Module (IR.Module Desugar) where
  desugar = desugarModule
