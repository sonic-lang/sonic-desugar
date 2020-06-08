{-# LANGUAGE PatternSynonyms #-}

module Language.Sonic.Compiler.Desugar.Type
  ( desugarType
  , desugarTypeInfix
  , desugarTyVarBinder
  , desugarContext
  , desugarPredicate
  )
where

import           Data.Foldable                  ( foldlM )

import qualified Language.Sonic.Parser         as Syn
                                                ( Position(..) )
import qualified Language.Sonic.Syntax.Sequence
                                               as Syn
                                                ( Sequence(..) )
import           Language.Sonic.Compiler.Context
                                                ( FileContext(..) )
import           Language.Sonic.Compiler.Provenance
                                                ( WithProv(..)
                                                , Prov(..)
                                                )
import           Language.Sonic.Compiler.Path   ( plainPath )
import qualified Language.Sonic.Compiler.Path.Constant
                                               as C
                                                ( tupleTyCtor )
import           Language.Sonic.Compiler.Desugar.Internal
                                                ( pattern SpanLoc
                                                , withSourceProv
                                                , withSourceProvSeq
                                                , parsedAt
                                                )
import           Language.Sonic.Compiler.Desugar.IR.Pass
                                                ( Desugar
                                                , passDesugar
                                                )

import qualified Language.Sonic.Syntax.Type    as Syn
                                                ( Type(..)
                                                , TypeInfix(..)
                                                , TyVarBinder(..)
                                                , Predicate(..)
                                                , Context(..)
                                                )
import qualified Language.Sonic.Compiler.IR.Type
                                               as IR
                                                ( TyVarBinder(..)
                                                , Predicate(..)
                                                , Context(..)
                                                )
import qualified Language.Sonic.Compiler.Desugar.IR.Type
                                               as IR

import           Language.Sonic.Compiler.Desugar.Name
                                                ( desugarTyVarName
                                                , desugarTyCtorName
                                                , desugarClassName
                                                )
import           Language.Sonic.Compiler.Desugar.Path
                                                ( desugarPath )
import           Language.Sonic.Compiler.Desugar.Kind
                                                ( desugarKind )

desugarType :: FileContext m => Syn.Type Syn.Position -> m IR.Type
desugarType (Syn.Parens x) = IR.Parens <$> withSourceProv desugarType x
desugarType (Syn.Var x) =
  IR.Var <$> withSourceProv (pure . plainPath . desugarTyVarName) x
desugarType (Syn.Ctor x) =
  IR.Ctor <$> withSourceProv (pure . desugarPath desugarTyCtorName) x
desugarType (Syn.Tuple (SpanLoc tupleSpan (Syn.Sequence xs))) = do
  ctorProv <- Derived passDesugar <$> parsedAt tupleSpan
  let ctor = IR.Ctor (WithProv ctorProv ctorPath)
  foldlM go ctor xs
 where
  go acc el = do
    tupleProv <- Derived passDesugar <$> parsedAt tupleSpan
    el'       <- withSourceProv desugarType el
    pure $ IR.Apply (WithProv tupleProv acc) el'
  ctorPath = C.tupleTyCtor arity
  arity    = length xs
desugarType (Syn.Apply lhs rhs) = do
  lhs' <- withSourceProv desugarType lhs
  rhs' <- withSourceProv desugarType rhs
  pure $ IR.Apply lhs' rhs'
desugarType (Syn.InfixApply lhs op rhs) = do
  lhs' <- withSourceProv desugarType lhs
  op'  <- withSourceProv (pure . desugarTypeInfix) op
  rhs' <- withSourceProv desugarType rhs
  pure $ IR.Infix lhs' op' rhs'
desugarType (Syn.Annotate type_ kind) = do
  type' <- withSourceProv desugarType type_
  kind' <- withSourceProv desugarKind kind
  pure $ IR.Annotate type' kind'
desugarType (Syn.Forall (Syn.Sequence vs) mctx type_) = do
  vs'   <- mapM (withSourceProv desugarTyVarBinder) vs
  mctx' <- mapM (withSourceProv desugarContext) mctx
  type' <- withSourceProv desugarType type_
  pure $ IR.Forall vs' mctx' type'

desugarTypeInfix :: Syn.TypeInfix Syn.Position -> IR.TypeInfix
desugarTypeInfix (Syn.TypeInfix op) =
  IR.TypeInfix (desugarPath desugarTyCtorName op)

desugarTyVarBinder
  :: FileContext m => Syn.TyVarBinder Syn.Position -> m (IR.TyVarBinder Desugar)
desugarTyVarBinder (Syn.TyVarBinder var mkind) = do
  var'   <- withSourceProv (pure . desugarTyVarName) var
  mkind' <- mapM (withSourceProv desugarKind) mkind
  pure $ IR.TyVarBinder var' mkind'

desugarContext
  :: FileContext m => Syn.Context Syn.Position -> m (IR.Context Desugar)
desugarContext (Syn.Context ps) =
  IR.Context <$> withSourceProvSeq desugarPredicate ps

desugarPredicate
  :: FileContext m => Syn.Predicate Syn.Position -> m (IR.Predicate Desugar)
desugarPredicate (Syn.Class cls (Syn.Sequence tys)) = do
  cls' <- withSourceProv (pure . desugarPath desugarClassName) cls
  tys' <- mapM (withSourceProv desugarType) tys
  pure $ IR.Class cls' tys'
desugarPredicate (Syn.Equality lhs rhs) = do
  lhs' <- withSourceProv desugarType lhs
  rhs' <- withSourceProv desugarType rhs
  pure $ IR.Equality lhs' rhs'
