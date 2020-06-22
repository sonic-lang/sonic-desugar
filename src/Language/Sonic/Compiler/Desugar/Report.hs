{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds             #-}

module Language.Sonic.Compiler.Desugar.Report
  ( EmptyFunctionClauses(..)
  , FunctionArgumentLengthMismatch(..)
  , StandaloneTypeSignature(..)
  , DuplicatedTypeSignature(..)
  , DuplicatedBindings(..)
  , Reports
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
  deriving stock (Eq, Show, Generic)
  deriving anyclass Report

data FunctionArgumentLengthMismatch
  = FunctionArgumentLengthMismatch
  { patsProv :: Prov
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass Report

data StandaloneTypeSignature
  = StandaloneTypeSignature
  { signatureDeclProv :: Prov
  , declaredName :: Name Var
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass Report

data DuplicatedTypeSignature
  = DuplicatedTypeSignature
  { signatureDeclProvs :: [Prov]
  , declaredName :: Name Var
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass Report

data DuplicatedBindings
  = DuplicatedBindings
  { bindingDeclProvs :: [Prov]
  , declaredName :: Name Var
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass Report

type Reports
  = '[EmptyFunctionClauses, FunctionArgumentLengthMismatch, StandaloneTypeSignature, DuplicatedTypeSignature, DuplicatedBindings]
