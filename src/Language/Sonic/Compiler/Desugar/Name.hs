module Language.Sonic.Compiler.Desugar.Name
  ( desugarCtorName
  , desugarVarName
  , desugarTyCtorName
  , desugarTyVarName
  , desugarClassName
  , desugarModuleComponentName
  )
where

import qualified Language.Sonic.Parser         as Syn
                                                ( Position(..) )
import qualified Language.Sonic.Syntax.Name    as Syn
                                                ( CtorName(..)
                                                , VarName(..)
                                                , TyCtorName(..)
                                                , TyVarName(..)
                                                , ClassName(..)
                                                , ModuleComponentName(..)
                                                )
import           Language.Sonic.Compiler.IR.EntityKind
                                                ( Ctor
                                                , Var
                                                , TyVar
                                                , TyCtor
                                                , Class
                                                , Module
                                                )

import qualified Language.Sonic.Compiler.Path  as IR
                                                ( Name(..) )

desugarCtorName :: Syn.CtorName Syn.Position -> IR.Name Ctor
desugarCtorName (Syn.CtorName t) = IR.Name t

desugarVarName :: Syn.VarName Syn.Position -> IR.Name Var
desugarVarName (Syn.VarName t) = IR.Name t

desugarTyVarName :: Syn.TyVarName Syn.Position -> IR.Name TyVar
desugarTyVarName (Syn.TyVarName t) = IR.Name t

desugarTyCtorName :: Syn.TyCtorName Syn.Position -> IR.Name TyCtor
desugarTyCtorName (Syn.TyCtorName t) = IR.Name t

desugarClassName :: Syn.ClassName Syn.Position -> IR.Name Class
desugarClassName (Syn.ClassName t) = IR.Name t

desugarModuleComponentName :: Syn.ModuleComponentName Syn.Position -> IR.Name Module
desugarModuleComponentName (Syn.ModuleComponentName t) = IR.Name t
