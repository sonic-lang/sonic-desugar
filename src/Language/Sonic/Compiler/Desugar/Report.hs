{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Sonic.Compiler.Desugar.Report
  ( EmptyFunctionClauses(..)
  , FunctionArgumentLengthMismatch(..)
  , StandaloneTypeSignature(..)
  , DuplicatedTypeSignature(..)
  , DuplicatedBindings(..)
  )
where

import           GHC.Generics                   ( Generic )

import           Language.Sonic.Compiler.Report ( Report )
import           Language.Sonic.Compiler.Provenance
                                                ( Prov )
import           Language.Sonic.Compiler.Path   ( Name )
import           Language.Sonic.Compiler.IR.EntityKind
                                                ( Var )

data EmptyFunctionClauses
  = EmptyFunctionClauses
  { clausesProv :: Prov
  }
  deriving stock Generic
  deriving anyclass Report

data FunctionArgumentLengthMismatch
  = FunctionArgumentLengthMismatch
  { patsProv :: Prov
  }
  deriving stock Generic
  deriving anyclass Report

data StandaloneTypeSignature
  = StandaloneTypeSignature
  { signatureDeclProv :: Prov
  , declaredName :: Name Var
  }
  deriving stock Generic
  deriving anyclass Report

data DuplicatedTypeSignature
  = DuplicatedTypeSignature
  { signatureDeclProvs :: [Prov]
  , declaredName :: Name Var
  }
  deriving stock Generic
  deriving anyclass Report

data DuplicatedBindings
  = DuplicatedBindings
  { bindingDeclProvs :: [Prov]
  , declaredName :: Name Var
  }
  deriving stock Generic
  deriving anyclass Report
