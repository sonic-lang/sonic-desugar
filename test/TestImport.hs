{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE GADTs                      #-}

module TestImport
  ( TestDesugar
  , runTestDesugar
  , TestReport
  )
where

import           Control.Monad.Trans.RWS        ( RWS
                                                , evalRWS
                                                , ask
                                                , get
                                                , put
                                                , tell
                                                )
import           GHC.Generics                   ( Generic )
import           Data.Type.Equality             ( (:~:)(..) )
import           Type.Membership                ( Forall(..)
                                                , Proxy(..)
                                                , Membership
                                                , compareMembership
                                                )
import           Type.Membership.Internal       ( Member(..) )
import           Language.Sonic.Compiler.Report ( Severity
                                                , MonadReport(..)
                                                )
import           Language.Sonic.Compiler.Unique ( MonadUnique(..) )
import           Language.Sonic.Compiler.Context
                                                ( FileContext(..) )
import           Language.Sonic.Compiler.Desugar
                                                ( Reports )

data TestReport = TestReport Severity String
  deriving (Show, Generic)

newtype TestDesugar a = TestDesugar (RWS FilePath [TestReport] Int a)
  deriving newtype (Functor, Applicative, Monad)

instance FileContext TestDesugar where
  currentFile = TestDesugar ask

instance MonadUnique TestDesugar where
  uniqueInt = TestDesugar $ do
    i <- get
    put (i + 1)
    pure i

instance Forall Show Reports => MonadReport Reports TestDesugar where
  report :: forall r . Member Reports r => Severity -> r -> TestDesugar ()
  report s r = henumerateFor (Proxy @Show) (Proxy @Reports) f (pure ())
   where
    f
      :: forall a
       . Show a
      => Membership Reports a
      -> TestDesugar ()
      -> TestDesugar ()
    f member acc = case compareMembership member (membership @Reports @r) of
      Left  _    -> acc
      Right Refl -> acc >> TestDesugar (tell [TestReport s (show r)])

runTestDesugar :: FilePath -> TestDesugar a -> (a, [TestReport])
runTestDesugar path (TestDesugar m) = evalRWS m path 0
