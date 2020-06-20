{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE ViewPatterns    #-}

module Language.Sonic.Compiler.Desugar.Internal
  ( pattern SpanLoc
  , pattern DiscardLoc
  , discardLoc
  , generatedProv
  , generated
  , withSourceProvSeq
  , withSourceProv
  , parsedAt
  -- * Generic supplement functions
  , foldMapM
  , desugarMaybeWithProv
  )
where

import           Data.Foldable                  ( foldlM )
import           Control.Monad.Trans.State.Strict
                                                ( StateT
                                                , get
                                                , put
                                                )

import qualified Language.Sonic.Parser         as Syn
                                                ( Position(..) )
import qualified Language.Sonic.Syntax.Sequence
                                               as Syn
                                                ( Sequence(..) )
import qualified Language.Sonic.Syntax.Location
                                               as Syn
                                                ( Located
                                                , L(..)
                                                )
import           Language.Sonic.Compiler.Context
                                                ( FileContext(..) )
import           Language.Sonic.Compiler.Location
                                                ( Location(..)
                                                , Span(..)
                                                , Position(..)
                                                , Line(..)
                                                , Column(..)
                                                )
import           Language.Sonic.Compiler.Provenance
                                                ( WithProv(..)
                                                , Prov(..)
                                                , Source(..)
                                                )

import           Language.Sonic.Compiler.Desugar.IR.Pass
                                                ( passDesugar )

generatedProv :: Prov
generatedProv = Source $ Generated passDesugar

generated :: a -> WithProv a
generated = WithProv generatedProv

withSourceProvSeq
  :: FileContext m
  => (a Syn.Position -> m b)
  -> Syn.Located Syn.Position (Syn.Sequence a)
  -> m (WithProv [WithProv b])
withSourceProvSeq f = withSourceProv g
  where g (Syn.Sequence xs) = mapM (withSourceProv f) xs

withSourceProv
  :: FileContext m
  => (a Syn.Position -> m b)
  -> Syn.Located Syn.Position a
  -> m (WithProv b)
withSourceProv f (SpanLoc sp x) = do
  prov <- parsedAt sp
  x'   <- f x
  pure $ WithProv prov x'

parsedAt :: FileContext m => Span -> m Prov
parsedAt sp = do
  file <- currentFile
  pure . Source . Parsed $ Location file sp

pattern SpanLoc :: Span -> a Syn.Position -> Syn.Located Syn.Position a
pattern SpanLoc sp x <- (unwrapSpan -> (sp, x))

{-# COMPLETE SpanLoc #-}

pattern DiscardLoc :: a Syn.Position -> Syn.Located Syn.Position a
pattern DiscardLoc x <- Syn.L _ x _

{-# COMPLETE DiscardLoc #-}

discardLoc :: Syn.Located Syn.Position a -> a Syn.Position
discardLoc (DiscardLoc x) = x

unwrapSpan :: Syn.Located Syn.Position a -> (Span, a Syn.Position)
unwrapSpan (Syn.L begin a end) = (sp, a)
  where sp = Span { begin = toPosition begin, end = toPosition end }

toPosition :: Syn.Position -> Position
toPosition Syn.Position { Syn.line, Syn.column } =
  Position { line = Line line, column = Column column }

foldMapM :: (Monad m, Monoid b, Foldable t) => (a -> m b) -> t a -> m b
foldMapM f = foldlM g mempty
 where
  g acc x = do
    x' <- f x
    pure $! acc `mappend` x'

desugarMaybeWithProv
  :: (Applicative m, Monoid ir)
  => (syn Syn.Position -> m (WithProv ir))
  -> Maybe (syn Syn.Position)
  -> m (WithProv ir)
desugarMaybeWithProv _ Nothing  = pure $ generated mempty
desugarMaybeWithProv f (Just x) = f x
