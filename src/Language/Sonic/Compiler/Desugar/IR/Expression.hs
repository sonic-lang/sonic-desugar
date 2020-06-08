{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Sonic.Compiler.Desugar.IR.Expression
  ( Expr
  , ExprInfix(..)
  , pattern Var
  , pattern Ctor
  , pattern Literal
  , pattern Apply
  , pattern Lambda
  , pattern Annotate
  , pattern Let
  , pattern Case
  , pattern Parens
  , pattern Infix
  )
where

import           GHC.Generics                   ( Generic )

import           Language.Sonic.Compiler.IR.Tree
                                                ( XRefID
                                                , XDefID
                                                , XWrap
                                                )
import qualified Language.Sonic.Compiler.IR.EntityKind
                                               as Entity
                                                ( Var
                                                , Ctor
                                                )
import           Language.Sonic.Compiler.IR.Literal
                                                ( Literal )
import           Language.Sonic.Compiler.IR.Expression
                                                ( XVar
                                                , XCtor
                                                , XLiteral
                                                , XApply
                                                , XLambda
                                                , XAnnotate
                                                , XLet
                                                , XCase
                                                , XXExpr
                                                )
import qualified Language.Sonic.Compiler.IR.Expression
                                               as IR
                                                ( Expr(..)
                                                , CaseArm
                                                , BindGroup
                                                )

import           Language.Sonic.Compiler.Desugar.IR.Type
                                                ( Type )
import           Language.Sonic.Compiler.Desugar.IR.Pass
                                                ( Desugar )

type instance XVar Desugar = ()
type instance XCtor Desugar = ()
type instance XLiteral Desugar = ()
type instance XApply Desugar = ()
type instance XLambda Desugar = ()
type instance XAnnotate Desugar = ()
type instance XLet Desugar = ()
type instance XCase Desugar = ()
type instance XXExpr Desugar = XExprDesugar

data XExprDesugar
  = XParens (XWrap Desugar Expr)
  | XInfix (XWrap Desugar Expr) (XWrap Desugar ExprInfix) (XWrap Desugar Expr)
  deriving Generic

data ExprInfix
  = VarInfix (XRefID Entity.Var Desugar)
  | CtorInfix (XRefID Entity.Ctor Desugar)
  deriving Generic

type Expr = IR.Expr Desugar
type BindGroup = IR.BindGroup Desugar
type CaseArm = IR.CaseArm Desugar

pattern Var :: XWrap Desugar (XRefID Entity.Var Desugar) -> Expr
pattern Var x = IR.Var () x

pattern Ctor :: XWrap Desugar (XRefID Entity.Ctor Desugar) -> Expr
pattern Ctor x = IR.Ctor () x

pattern Literal :: XWrap Desugar Literal -> Expr
pattern Literal x = IR.Literal () x

pattern Apply :: XWrap Desugar Expr -> XWrap Desugar Expr -> Expr
pattern Apply x y = IR.Apply () x y

pattern Lambda :: XWrap Desugar (XDefID Entity.Var Desugar) -> XWrap Desugar Expr -> Expr
pattern Lambda v e = IR.Lambda () v e

pattern Annotate :: XWrap Desugar Expr -> XWrap Desugar Type -> Expr
pattern Annotate x y = IR.Annotate () x y

pattern Let :: XWrap Desugar [XWrap Desugar BindGroup] -> XWrap Desugar Expr -> Expr
pattern Let x y = IR.Let () x y

pattern Case :: XWrap Desugar Expr -> XWrap Desugar [XWrap Desugar CaseArm] -> Expr
pattern Case x y = IR.Case () x y

pattern Parens :: XWrap Desugar Expr -> Expr
pattern Parens x = IR.XExpr (XParens x)

pattern Infix :: XWrap Desugar Expr -> XWrap Desugar ExprInfix -> XWrap Desugar Expr -> Expr
pattern Infix x op y = IR.XExpr (XInfix x op y)
