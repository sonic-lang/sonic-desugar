{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

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
import qualified Language.Sonic.Compiler.IR.Module
                                               as IR
                                                ( Module(..) )
import qualified Language.Sonic.Compiler.Desugar.IR.Declaration
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
  { _stateAttrs :: [WithProv (IR.Attrs Desugar)]
  , _stateDecls :: [WithProv IR.Decl]
  -- | collect 'Syn.SimpleDecl' separately to process them later at once
  , _stateSimpleDecls :: [SimpleDeclSource]
  }

$(makeLenses ''DesugarModuleState)

desugarModule
  :: (FileContext m, MonadUnique m, MonadReport m)
  => Syn.Module Syn.Position
  -> m (IR.Module Desugar)
desugarModule (Syn.Module (Syn.Sequence items)) = do
  st    <- execStateT (mapM_ (desugarModuleItem . discardLoc) items) initState
  binds <- desugarSimpleDecls (st ^. stateSimpleDecls)
  let decl = generated (IR.Bind (generated (IR.BindGroup binds)))
  pure IR.Module { IR.attrs = st ^. stateAttrs
                 , IR.decls = decl : st ^. stateDecls
                 }

initState :: DesugarModuleState
initState = DesugarModuleState [] [] []

desugarModuleItem
  :: (FileContext m, MonadUnique m, MonadReport m)
  => Syn.ModuleItem Syn.Position
  -> StateT DesugarModuleState m ()
desugarModuleItem (Syn.TopAttr attrs) = do
  attrs' <- lift $ withSourceProv desugarAttrSet attrs
  stateAttrs <>= [attrs']
desugarModuleItem (Syn.TopDecl (DiscardLoc decl)) = desugarTopDecl decl

desugarTopDecl
  :: (FileContext m, MonadUnique m, MonadReport m)
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
  stateDecls <>= [WithProv declProv (IR.Data (WithProv declProv decl'))]
desugarTopDecl (Syn.WithAttrSet attrs (SpanLoc declSpan (Syn.Class decl))) = do
  attrs'   <- lift $ desugarMaybeWithProv (withSourceProv desugarAttrSet) attrs
  declProv <- lift $ parsedAt declSpan
  decl'    <- lift $ desugarClassDecl attrs' decl
  stateDecls <>= [WithProv declProv (IR.Class (WithProv declProv decl'))]
desugarTopDecl (Syn.WithAttrSet attrs (SpanLoc declSpan (Syn.Instance decl))) =
  do
    attrs' <- lift $ desugarMaybeWithProv (withSourceProv desugarAttrSet) attrs
    declProv <- lift $ parsedAt declSpan
    decl' <- lift $ desugarInstanceDecl attrs' decl
    stateDecls <>= [WithProv declProv (IR.Instance (WithProv declProv decl'))]
