module Language.Sonic.Compiler.Desugar
  ( desugarModule
  , Desugarable(..)
  -- * Pass
  , module Pass
  -- * Reporting
  , module Report
  )
where

import           Language.Sonic.Compiler.Desugar.IR.Pass
                                               as Pass

import           Language.Sonic.Compiler.Desugar.Report
                                               as Report

import           Language.Sonic.Compiler.Desugar.Module
                                                ( desugarModule )
import           Language.Sonic.Compiler.Desugar.Desugarable
                                                ( Desugarable(..) )
