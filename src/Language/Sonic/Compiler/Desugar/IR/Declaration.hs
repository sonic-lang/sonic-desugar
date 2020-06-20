{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Sonic.Compiler.Desugar.IR.Declaration
  ( Decl
  , pattern Bind
  , pattern Data
  , pattern Class
  , pattern Instance
  )
where

import           Data.Void                      ( Void )

import           Language.Sonic.Compiler.IR.Tree
                                                ( XWrap )
import           Language.Sonic.Compiler.IR.Declaration
                                                ( XBind
                                                , XData
                                                , XClass
                                                , XInstance
                                                , XXDecl
                                                )
import qualified Language.Sonic.Compiler.IR.Expression
                                               as IR
                                               ( BindGroup )
import qualified Language.Sonic.Compiler.IR.Declaration
                                               as IR
                                                ( Decl(..)
                                                , DataDecl
                                                , ClassDecl
                                                , InstanceDecl
                                                )

import           Language.Sonic.Compiler.Desugar.IR.Pass
                                                ( Desugar )

type instance XBind Desugar = ()
type instance XData Desugar = ()
type instance XClass Desugar = ()
type instance XInstance Desugar = ()
type instance XXDecl Desugar = Void

type Decl = IR.Decl Desugar

pattern Bind :: XWrap Desugar (IR.BindGroup Desugar) -> Decl
pattern Bind x = IR.Bind () x

pattern Data :: XWrap Desugar (IR.DataDecl Desugar) -> Decl
pattern Data x = IR.Data () x

pattern Class :: XWrap Desugar (IR.ClassDecl Desugar) -> Decl
pattern Class x = IR.Class () x

pattern Instance :: XWrap Desugar (IR.InstanceDecl Desugar) -> Decl
pattern Instance x = IR.Instance () x
