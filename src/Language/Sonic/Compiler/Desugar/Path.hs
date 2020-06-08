{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Language.Sonic.Compiler.Desugar.Path
  ( desugarPath
  , desugarPathPrefix
  )
where

import           Data.Foldable                  ( foldl' )

import qualified Language.Sonic.Parser         as Syn
                                                ( Position(..) )
import qualified Language.Sonic.Syntax.Sequence
                                               as Syn
                                                ( Sequence(..) )
import qualified Language.Sonic.Syntax.Path    as Syn
                                                ( Path(..)
                                                , PathPrefix(..)
                                                )

import           Language.Sonic.Compiler.Desugar.Internal
                                                ( pattern DiscardLoc
                                                , discardLoc
                                                )
import qualified Language.Sonic.Compiler.Path  as IR
                                                ( Path(..)
                                                , SimplePath(..)
                                                , PathPrefix(..)
                                                , Name
                                                )
import           Language.Sonic.Compiler.Desugar.Name
                                                ( desugarModuleComponentName )

desugarPathPrefix :: Syn.PathPrefix Syn.Position -> IR.PathPrefix
desugarPathPrefix Syn.Dot    = IR.Local
desugarPathPrefix Syn.Dollar = IR.Global
desugarPathPrefix Syn.Hash   = IR.Primitive

desugarPath
  :: (n Syn.Position -> IR.Name k) -> Syn.Path n Syn.Position -> IR.Path k
desugarPath f Syn.Path { Syn.prefix, Syn.modulePath, Syn.name = DiscardLoc name }
  = IR.Path { IR.prefix = fmap (desugarPathPrefix . discardLoc) prefix
            , IR.inner
            }
 where
  inner = IR.SimplePath (desugarModulePath modulePath) (f name)
  desugarModulePath Nothing = Nothing
  desugarModulePath (Just (DiscardLoc (Syn.Sequence xs))) =
    foldl' go Nothing xs
  go acc (DiscardLoc x) =
    Just (IR.SimplePath acc (desugarModuleComponentName x))
