{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Sonic.Compiler.Desugar.Expression
  ( desugarExpr
  , desugarExprInfix
  , desugarLetDefn
  , desugarCaseArm
  , desugarGuard
  )
where

import           Data.Foldable                  ( foldlM
                                                , foldrM
                                                )

import qualified Language.Sonic.Parser         as Syn
                                                ( Position(..) )
import qualified Language.Sonic.Syntax.Sequence
                                               as Syn
                                                ( Sequence(..) )
import           Language.Sonic.Compiler.Context
                                                ( FileContext(..) )
import           Language.Sonic.Compiler.Unique ( MonadUnique )
import           Language.Sonic.Compiler.Provenance
                                                ( WithProv(..)
                                                , Prov(..)
                                                )
import           Language.Sonic.Compiler.Path   ( Name
                                                , newName
                                                , localPath
                                                )
import qualified Language.Sonic.Compiler.Path.Constant
                                               as C
                                                ( tupleCtor )
import           Language.Sonic.Compiler.IR.EntityKind
                                                ( Var )
import           Language.Sonic.Compiler.IR.Pattern
                                                ( replace
                                                , collectVars
                                                )
import           Language.Sonic.Compiler.Desugar.Internal
                                                ( pattern SpanLoc
                                                , pattern DiscardLoc
                                                , withSourceProv
                                                , withSourceProvSeq
                                                , generated
                                                , parsedAt
                                                )
import           Language.Sonic.Compiler.Desugar.IR.Pass
                                                ( Desugar
                                                , passDesugar
                                                )

import qualified Language.Sonic.Syntax.Pattern as SynPat
                                                ( Pat(..) )
import qualified Language.Sonic.Syntax.Expression
                                               as Syn
                                                ( Expr(..)
                                                , ExprInfix(..)
                                                , LetDefn(..)
                                                , LetBinder(..)
                                                , CaseArm(..)
                                                , Guard(..)
                                                )
import qualified Language.Sonic.Compiler.Desugar.IR.Pattern
                                               as IRPat
                                                ( Pat
                                                , pattern Wildcard
                                                )
import qualified Language.Sonic.Compiler.IR.Expression
                                               as IR
                                                ( CaseArm(..)
                                                , Bind(..)
                                                , BindGroup(..)
                                                )
import qualified Language.Sonic.Compiler.Desugar.IR.Expression
                                               as IR

import           Language.Sonic.Compiler.Desugar.Literal
                                                ( desugarLiteral )
import           Language.Sonic.Compiler.Desugar.Name
                                                ( desugarVarName
                                                , desugarCtorName
                                                )
import           Language.Sonic.Compiler.Desugar.Path
                                                ( desugarPath )
import           Language.Sonic.Compiler.Desugar.Type
                                                ( desugarType )
import           Language.Sonic.Compiler.Desugar.Pattern
                                                ( desugarPat )

desugarExpr
  :: (FileContext m, MonadUnique m) => Syn.Expr Syn.Position -> m IR.Expr
desugarExpr (Syn.Parens x) = IR.Parens <$> withSourceProv desugarExpr x
desugarExpr (Syn.Var x) =
  IR.Var <$> withSourceProv (pure . desugarPath desugarVarName) x
desugarExpr (Syn.Ctor x) =
  IR.Ctor <$> withSourceProv (pure . desugarPath desugarCtorName) x
desugarExpr (Syn.Literal x) =
  IR.Literal <$> withSourceProv (pure . desugarLiteral) x
desugarExpr (Syn.Tuple (SpanLoc tupleSpan (Syn.Sequence xs))) = do
  ctorProv <- Derived passDesugar <$> parsedAt tupleSpan
  let ctor = IR.Ctor (WithProv ctorProv ctorPath)
  foldlM go ctor xs
 where
  go acc el = do
    tupleProv <- Derived passDesugar <$> parsedAt tupleSpan
    el'       <- withSourceProv desugarExpr el
    pure $ IR.Apply (WithProv tupleProv acc) el'
  ctorPath = C.tupleCtor arity
  arity    = length xs
desugarExpr (Syn.Apply lhs rhs) = do
  lhs' <- withSourceProv desugarExpr lhs
  rhs' <- withSourceProv desugarExpr rhs
  pure $ IR.Apply lhs' rhs'
desugarExpr (Syn.InfixApply lhs op rhs) = do
  lhs' <- withSourceProv desugarExpr lhs
  op'  <- withSourceProv (pure . desugarExprInfix) op
  rhs' <- withSourceProv desugarExpr rhs
  pure $ IR.Infix lhs' op' rhs'
desugarExpr (Syn.Lambda (Syn.Sequence pats) body) = do
  body'        <- withSourceProv desugarExpr body
  WithProv _ x <- foldrM go body' pats
  pure x
 where
  go (DiscardLoc (SynPat.Var x)) acc = do
    x' <- withSourceProv (pure . desugarVarName) x
    pure . generated $ IR.Lambda x' acc
  go pat@(SpanLoc patSpan _) acc = do
    pat'    <- withSourceProv desugarPat pat
    armProv <- Derived passDesugar <$> parsedAt patSpan
    let arm = IR.CaseArm pat' Nothing acc
    var <- newName
    let case_ = IR.Case (generated (IR.Var (generated (localPath var))))
                        (generated [WithProv armProv arm])
    pure . generated $ IR.Lambda (generated var) (generated case_)
desugarExpr (Syn.Annotate expr type_) = do
  expr' <- withSourceProv desugarExpr expr
  type' <- withSourceProv desugarType type_
  pure $ IR.Annotate expr' type'
desugarExpr (Syn.Let defns body) = do
  binds <- withSourceProv desugarLetDefns defns
  body' <- withSourceProv desugarExpr body
  pure $ IR.Let (generated [fmap IR.BindGroup binds]) body'
 where
  desugarLetDefns (Syn.Sequence ds) = concat <$> mapM go ds
  go (SpanLoc defnSpan defn) = do
    binds    <- desugarLetDefn defn
    bindProv <- parsedAt defnSpan
    pure $ map (WithProv bindProv) binds
desugarExpr (Syn.Case expr arms) = do
  expr' <- withSourceProv desugarExpr expr
  arms' <- withSourceProvSeq desugarCaseArm arms
  pure $ IR.Case expr' arms'

desugarExprInfix :: Syn.ExprInfix Syn.Position -> IR.ExprInfix
desugarExprInfix (Syn.VarInfix v) = IR.VarInfix $ desugarPath desugarVarName v
desugarExprInfix (Syn.CtorInfix c) =
  IR.CtorInfix $ desugarPath desugarCtorName c

desugarLetDefn
  :: (FileContext m, MonadUnique m)
  => Syn.LetDefn Syn.Position
  -> m [IR.Bind Desugar]
desugarLetDefn (Syn.LetDefn (DiscardLoc (Syn.AnnotatedBinder var type_)) body)
  = do
    var'  <- withSourceProv (pure . desugarVarName) var
    type' <- withSourceProv desugarType type_
    body' <- withSourceProv desugarExpr body
    pure [IR.Bind var' (Just type') body']
desugarLetDefn (Syn.LetDefn (DiscardLoc (Syn.PatBinder pat)) body) = do
  body'       <- withSourceProv desugarExpr body
  rhsTempName <- newName
  let bind1 = IR.Bind (generated rhsTempName) Nothing body'
  pat' <- withSourceProv desugarPat pat
  let binds = desugarLetDefnPatBinds pat' rhsTempName
  pure (bind1 : binds)

desugarLetDefnPatBinds :: WithProv IRPat.Pat -> Name Var -> [IR.Bind Desugar]
desugarLetDefnPatBinds (WithProv patProv pat) rhsName = binds
 where
  binds        = map makeVarBind $ collectVars pat
  replacedProv = Derived passDesugar patProv
  makeVarBind v = IR.Bind v Nothing (generated (makeExtractCase v))
  makeExtractCase v = IR.Case
    (generated (IR.Var (generated (localPath rhsName))))
    (generated [generated (makeExtractArm v)])
  makeExtractArm (WithProv varProv var) = IR.CaseArm
    (WithProv replacedProv (replace var IRPat.Wildcard pat))
    Nothing
    (generated (IR.Var (WithProv varProv (localPath var))))

desugarCaseArm
  :: (FileContext m, MonadUnique m)
  => Syn.CaseArm Syn.Position
  -> m (IR.CaseArm Desugar)
desugarCaseArm (Syn.CaseArm pat guard body) = do
  pat'   <- withSourceProv desugarPat pat
  guard' <- mapM (withSourceProv desugarGuard) guard
  body'  <- withSourceProv desugarExpr body
  pure $ IR.CaseArm pat' guard' body'

desugarGuard
  :: (FileContext m, MonadUnique m) => Syn.Guard Syn.Position -> m IR.Expr
desugarGuard (Syn.Guard g) = do
  WithProv _ e <- withSourceProv desugarExpr g
  pure e
