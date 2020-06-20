{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Sonic.Compiler.Desugar.IR.Module
  ( pattern Module
  , attrs
  , bindings
  , dataDecls
  , classDecls
  , instanceDecls
  )
where

import           Language.Sonic.Compiler.IR.Tree
                                                ( XWrap )
import           Language.Sonic.Compiler.IR.Attribute
                                                ( Attrs )
import           Language.Sonic.Compiler.IR.Expression
                                                ( BindGroup )
import           Language.Sonic.Compiler.IR.Declaration
                                                ( DataDecl
                                                , ClassDecl
                                                , InstanceDecl
                                                )
import           Language.Sonic.Compiler.IR.Module
                                                ( XModule )
import qualified Language.Sonic.Compiler.IR.Module
                                               as IR
                                                ( Module(..) )

import           Language.Sonic.Compiler.Desugar.IR.Pass
                                                ( Desugar )

type instance XModule Desugar = ()

pattern Module
  :: [XWrap Desugar (Attrs Desugar)]
  -> [XWrap Desugar (BindGroup Desugar)]
  -> [XWrap Desugar (DataDecl Desugar)]
  -> [XWrap Desugar (ClassDecl Desugar)]
  -> [XWrap Desugar (InstanceDecl Desugar)]
  -> IR.Module Desugar
pattern Module { attrs, bindings, dataDecls, classDecls, instanceDecls } =
  IR.Module { IR.attrs, IR.bindings, IR.dataDecls, IR.classDecls, IR.instanceDecls, IR.extension = () }
