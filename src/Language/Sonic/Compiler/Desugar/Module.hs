{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Sonic.Compiler.Desugar.Module
  ( desugarModule
  )
where

import           Control.Monad.Trans.State.Strict
                                                ( StateT
                                                , execStateT
                                                )
import           Control.Monad.Trans.Class      ( lift )

import           Lens.Micro
import           Lens.Micro.TH                  ( makeLenses )

import qualified Language.Sonic.Parser         as Syn
                                                ( Position(..) )
import qualified Language.Sonic.Syntax.Sequence
                                               as Syn
                                                ( Sequence(..) )
import           Language.Sonic.Compiler.Context
                                                ( FileContext )
import           Language.Sonic.Compiler.Unique ( MonadUnique )
import           Language.Sonic.Compiler.Report ( MonadReport(..) )
import           Language.Sonic.Compiler.Provenance
                                                ( WithProv(..) )
import           Language.Sonic.Compiler.Desugar.Report
                                                ( Reports )
import           Language.Sonic.Compiler.Desugar.Internal
                                                ( pattern DiscardLoc
                                                , pattern SpanLoc
                                                , discardLoc
                                                , parsedAt
                                                , generated
                                                , withSourceProv
                                                , desugarMaybeWithProv
                                                , (<>=)
                                                )
import           Language.Sonic.Compiler.Desugar.IR.Pass
                                                ( Desugar )

import qualified Language.Sonic.Syntax.Attribute
                                               as Syn
                                                ( WithAttrSet(..) )
import qualified Language.Sonic.Syntax.Declaration
                                               as Syn
                                                ( Decl(..) )
import qualified Language.Sonic.Syntax.Module  as Syn
                                                ( Module(..)
                                                , ModuleItem(..)
                                                )
import qualified Language.Sonic.Compiler.IR.Attribute
                                               as IR
                                                ( Attrs )
import qualified Language.Sonic.Compiler.IR.Expression
                                               as IR
                                                ( BindGroup(..) )
import qualified Language.Sonic.Compiler.IR.Declaration
                                               as IR
                                                ( DataDecl
                                                , ClassDecl
                                                , InstanceDecl
                                                )
import qualified Language.Sonic.Compiler.IR.Module
                                               as IR
                                                ( Module )
import qualified Language.Sonic.Compiler.Desugar.IR.Module
                                               as IR

import           Language.Sonic.Compiler.Desugar.Attribute
                                                ( desugarAttrSet )
import           Language.Sonic.Compiler.Desugar.Declaration
                                                ( desugarDataDecl
                                                , desugarClassDecl
                                                , desugarInstanceDecl
                                                , desugarSimpleDecls
                                                , SimpleDeclSource(..)
                                                )

data DesugarModuleState
  = DesugarModuleState
  { _stateAttrs         :: [WithProv (IR.Attrs Desugar)]
  , _stateDataDecls     :: [WithProv (IR.DataDecl Desugar)]
  , _stateClassDecls    :: [WithProv (IR.ClassDecl Desugar)]
  , _stateInstanceDecls :: [WithProv (IR.InstanceDecl Desugar)]
  , _stateSimpleDecls   :: [SimpleDeclSource]
  }

$(makeLenses ''DesugarModuleState)

desugarModule
  :: (FileContext m, MonadUnique m, MonadReport Reports m)
  => Syn.Module Syn.Position
  -> m (IR.Module Desugar)
desugarModule (Syn.Module (Syn.Sequence items)) = do
  st    <- execStateT (mapM_ (desugarModuleItem . discardLoc) items) initState
  binds <- desugarSimpleDecls (st ^. stateSimpleDecls)
  pure IR.Module { IR.attrs         = st ^. stateAttrs
                 , IR.bindings      = [generated (IR.BindGroup binds)]
                 , IR.dataDecls     = st ^. stateDataDecls
                 , IR.classDecls    = st ^. stateClassDecls
                 , IR.instanceDecls = st ^. stateInstanceDecls
                 }

initState :: DesugarModuleState
initState = DesugarModuleState [] [] [] [] []

desugarModuleItem
  :: (FileContext m, MonadUnique m, MonadReport Reports m)
  => Syn.ModuleItem Syn.Position
  -> StateT DesugarModuleState m ()
desugarModuleItem (Syn.TopAttr attrs) = do
  attrs' <- lift $ withSourceProv desugarAttrSet attrs
  stateAttrs <>= [attrs']
desugarModuleItem (Syn.TopDecl (DiscardLoc decl)) = desugarTopDecl decl

desugarTopDecl
  :: (FileContext m, MonadUnique m, MonadReport Reports m)
  => Syn.WithAttrSet Syn.Decl Syn.Position
  -> StateT DesugarModuleState m ()
desugarTopDecl (Syn.WithAttrSet attrs (SpanLoc declSpan (Syn.Simple decl))) =
  do
    attrs' <- lift $ desugarMaybeWithProv (withSourceProv desugarAttrSet) attrs
    declProv <- lift $ parsedAt declSpan
    stateSimpleDecls <>= [SimpleDeclSource { attrs = attrs', declProv, decl }]
desugarTopDecl (Syn.WithAttrSet attrs (SpanLoc declSpan (Syn.Data decl))) = do
  attrs'   <- lift $ desugarMaybeWithProv (withSourceProv desugarAttrSet) attrs
  declProv <- lift $ parsedAt declSpan
  decl'    <- lift $ desugarDataDecl attrs' decl
  stateDataDecls <>= [WithProv declProv decl']
desugarTopDecl (Syn.WithAttrSet attrs (SpanLoc declSpan (Syn.Class decl))) = do
  attrs'   <- lift $ desugarMaybeWithProv (withSourceProv desugarAttrSet) attrs
  declProv <- lift $ parsedAt declSpan
  decl'    <- lift $ desugarClassDecl attrs' decl
  stateClassDecls <>= [WithProv declProv decl']
desugarTopDecl (Syn.WithAttrSet attrs (SpanLoc declSpan (Syn.Instance decl))) =
  do
    attrs' <- lift $ desugarMaybeWithProv (withSourceProv desugarAttrSet) attrs
    declProv <- lift $ parsedAt declSpan
    decl' <- lift $ desugarInstanceDecl attrs' decl
    stateInstanceDecls <>= [WithProv declProv decl']
