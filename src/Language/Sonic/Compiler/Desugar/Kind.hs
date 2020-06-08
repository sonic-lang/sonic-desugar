module Language.Sonic.Compiler.Desugar.Kind
  ( desugarKind
  )
where

import qualified Language.Sonic.Parser         as Syn
                                                ( Position )
import           Language.Sonic.Compiler.Context
                                                ( FileContext )
import           Language.Sonic.Compiler.Desugar.Internal
                                                ( withSourceProv )

import qualified Language.Sonic.Syntax.Kind    as Syn
                                                ( Kind(..) )
import qualified Language.Sonic.Compiler.Desugar.IR.Kind
                                               as IR

desugarKind :: FileContext m => Syn.Kind Syn.Position -> m IR.Kind
desugarKind Syn.Type            = pure IR.Type
desugarKind (Syn.Arrow lhs rhs) = do
  lhs' <- withSourceProv desugarKind lhs
  rhs' <- withSourceProv desugarKind rhs
  pure $ IR.Arrow lhs' rhs'
