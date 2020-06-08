module Language.Sonic.Compiler.Desugar.Literal
  ( desugarLiteral
  )
where

import qualified Language.Sonic.Parser         as Syn
                                                ( Position(..) )
import qualified Language.Sonic.Syntax.Literal as Syn
                                                ( Literal(..) )

import qualified Language.Sonic.Compiler.IR.Literal
                                               as IR
                                                ( Literal(..) )

desugarLiteral :: Syn.Literal Syn.Position -> IR.Literal
desugarLiteral (Syn.Integer i) = IR.Integer i
desugarLiteral (Syn.Char    c) = IR.Char c
