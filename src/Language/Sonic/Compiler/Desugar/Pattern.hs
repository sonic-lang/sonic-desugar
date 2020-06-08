{-# LANGUAGE PatternSynonyms #-}

module Language.Sonic.Compiler.Desugar.Pattern
  ( desugarPat
  , desugarPatInfix
  )
where

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
import qualified Language.Sonic.Compiler.Path.Constant
                                               as C
                                                ( tupleCtor )
import           Language.Sonic.Compiler.Desugar.Internal
                                                ( pattern SpanLoc
                                                , withSourceProv
                                                , parsedAt
                                                )
import           Language.Sonic.Compiler.Desugar.IR.Pass
                                                ( passDesugar )

import qualified Language.Sonic.Syntax.Pattern as Syn
                                                ( Pat(..)
                                                , PatInfix(..)
                                                )
import qualified Language.Sonic.Compiler.Desugar.IR.Pattern
                                               as IR

import           Language.Sonic.Compiler.Desugar.Literal
                                                ( desugarLiteral )
import           Language.Sonic.Compiler.Desugar.Name
                                                ( desugarVarName
                                                , desugarCtorName
                                                )
import           Language.Sonic.Compiler.Desugar.Path
                                                ( desugarPath )

desugarPat :: FileContext m => Syn.Pat Syn.Position -> m IR.Pat
desugarPat (Syn.Parens x) = IR.Parens <$> withSourceProv desugarPat x
desugarPat Syn.Wildcard   = pure IR.Wildcard
desugarPat (Syn.Literal l) =
  IR.Literal <$> withSourceProv (pure . desugarLiteral) l
desugarPat (Syn.Var v) = IR.Var <$> withSourceProv (pure . desugarVarName) v
desugarPat (Syn.Tuple (SpanLoc sp (Syn.Sequence ps))) = do
  ctorProv <- Derived passDesugar <$> parsedAt sp
  ps'      <- mapM (withSourceProv desugarPat) ps
  pure $ IR.Ctor (WithProv ctorProv ctorPath) ps'
  where ctorPath = C.tupleCtor (length ps)
desugarPat (Syn.Ctor ctor (Syn.Sequence ps)) = do
  ctor' <- withSourceProv (pure . desugarPath desugarCtorName) ctor
  ps'   <- mapM (withSourceProv desugarPat) ps
  pure $ IR.Ctor ctor' ps'
desugarPat (Syn.Infix lhs op rhs) = do
  lhs' <- withSourceProv desugarPat lhs
  op'  <- withSourceProv (pure . desugarPatInfix) op
  rhs' <- withSourceProv desugarPat rhs
  pure $ IR.Infix lhs' op' rhs'

desugarPatInfix :: Syn.PatInfix Syn.Position -> IR.PatInfix
desugarPatInfix (Syn.PatInfix path) =
  IR.PatInfix (desugarPath desugarCtorName path)
