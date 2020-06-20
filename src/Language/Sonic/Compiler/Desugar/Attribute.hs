{-# LANGUAGE PatternSynonyms #-}

module Language.Sonic.Compiler.Desugar.Attribute
  ( desugarAttrSet
  , desugarAttrValue
  , desugarAttrValueList
  )
where

import qualified Data.Map.Strict               as Map
                                                ( fromList )

import qualified Language.Sonic.Parser         as Syn
                                                ( Position(..) )
import qualified Language.Sonic.Syntax.Location
                                               as Syn
                                                ( L(..) )
import qualified Language.Sonic.Syntax.Sequence
                                               as Syn
                                                ( Sequence(..) )
import           Language.Sonic.Compiler.Context
                                                ( FileContext(..) )
import           Language.Sonic.Compiler.Provenance
                                                ( WithProv(..) )
import           Language.Sonic.Compiler.Desugar.Internal
                                                ( pattern SpanLoc
                                                , pattern DiscardLoc
                                                , withSourceProv
                                                , parsedAt
                                                )
import           Language.Sonic.Compiler.Desugar.IR.Pass
                                                ( Desugar )

import qualified Language.Sonic.Syntax.Name    as Syn
                                                ( EntityName(..)
                                                , ValueName(..)
                                                , TypeName(..)
                                                )
import qualified Language.Sonic.Syntax.Path    as Syn
                                                ( Path(..) )
import qualified Language.Sonic.Syntax.Attribute
                                               as Syn
                                                ( AttrSet(..)
                                                , Attr(..)
                                                , AttrValue(..)
                                                , AttrValueList(..)
                                                )
import qualified Language.Sonic.Compiler.IR.Attribute
                                               as IR
                                                ( Attrs(..)
                                                , Attr(..)
                                                , AttrValue(..)
                                                , EntityRef(..)
                                                )

import           Language.Sonic.Compiler.Desugar.Name
                                                ( desugarAttrKeyName
                                                , desugarClassName
                                                , desugarCtorName
                                                , desugarVarName
                                                , desugarTyCtorName
                                                , desugarTyVarName
                                                )
import           Language.Sonic.Compiler.Desugar.Path
                                                ( desugarPath )

desugarAttrSet
  :: FileContext m => Syn.AttrSet Syn.Position -> m (IR.Attrs Desugar)
desugarAttrSet (Syn.AttrSet (Syn.Sequence as)) = do
  as' <- mapM desugarAttr as
  pure . IR.Attrs $ Map.fromList as'
 where
  desugarAttr (SpanLoc s a) = do
    p      <- parsedAt s
    (k, v) <- desugarAttrR a
    pure (desugarAttrKeyName k, WithProv p v)
  desugarAttrR (Syn.Name (DiscardLoc k)   ) = pure (k, IR.Name)
  desugarAttrR (Syn.Value (DiscardLoc k) v) = do
    v' <- withSourceProv (pure . desugarAttrValue) v
    pure (k, IR.Value v')
  desugarAttrR (Syn.List (DiscardLoc k) vs) = do
    vs' <- withSourceProv desugarAttrValueList vs
    pure (k, IR.List vs')
  desugarAttrR (Syn.Record (DiscardLoc k) s) = do
    s' <- withSourceProv desugarAttrSet s
    pure (k, IR.Record s')

desugarAttrValue :: Syn.AttrValue Syn.Position -> IR.AttrValue Desugar
desugarAttrValue (Syn.TextValue txt ) = IR.TextValue txt
desugarAttrValue (Syn.PathValue path) = IR.RefValue $ desugarEntityPath path
 where
  desugarEntityPath
    :: Syn.Path Syn.EntityName Syn.Position -> IR.EntityRef Desugar
  desugarEntityPath (Syn.Path p m (DiscardLoc (Syn.ValueEntityName n))) =
    desugarValueEntityPath (Syn.Path p m n)
  desugarEntityPath (Syn.Path p m (DiscardLoc (Syn.TypeEntityName n))) =
    desugarTypeEntityPath (Syn.Path p m n)
  desugarEntityPath (Syn.Path p m (DiscardLoc (Syn.ClassEntityName n))) =
    IR.Class $ desugarPath desugarClassName (Syn.Path p m n)
  desugarValueEntityPath
    :: Syn.Path Syn.ValueName Syn.Position -> IR.EntityRef Desugar
  desugarValueEntityPath (Syn.Path p m (Syn.L b (Syn.CtorValueName n) e)) =
    IR.Ctor $ desugarPath desugarCtorName (Syn.Path p m (Syn.L b n e))
  desugarValueEntityPath (Syn.Path p m (Syn.L b (Syn.VarValueName n) e)) =
    IR.Var $ desugarPath desugarVarName (Syn.Path p m (Syn.L b n e))
  desugarTypeEntityPath
    :: Syn.Path Syn.TypeName Syn.Position -> IR.EntityRef Desugar
  desugarTypeEntityPath (Syn.Path p m (Syn.L b (Syn.CtorTypeName n) e)) =
    IR.TyCtor $ desugarPath desugarTyCtorName (Syn.Path p m (Syn.L b n e))
  desugarTypeEntityPath (Syn.Path p m (Syn.L b (Syn.VarTypeName n) e)) =
    IR.TyVar $ desugarPath desugarTyVarName (Syn.Path p m (Syn.L b n e))

desugarAttrValueList
  :: FileContext m
  => Syn.AttrValueList Syn.Position
  -> m [WithProv (IR.AttrValue Desugar)]
desugarAttrValueList (Syn.AttrValueList (Syn.Sequence vs)) =
  mapM (withSourceProv (pure . desugarAttrValue)) vs
