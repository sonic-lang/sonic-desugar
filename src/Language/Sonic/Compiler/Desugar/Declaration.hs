{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeFamilies          #-}

module Language.Sonic.Compiler.Desugar.Declaration
  ( desugarDataDecl
  , desugarClassDecl
  , desugarSimpleDecls
  , desugarInstanceDecl
  , SimpleDeclSource(..)
  )
where

import           Data.Functor                   ( ($>) )
import           Control.Monad                  ( unless
                                                , replicateM
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.State.Strict
                                                ( get
                                                , modify
                                                , execStateT
                                                )
import qualified Data.Map.Strict               as Map
                                                ( lookup
                                                , insert
                                                , empty
                                                )

import qualified Language.Sonic.Parser         as Syn
                                                ( Position(..) )
import qualified Language.Sonic.Syntax.Location
                                               as Syn
                                                ( Located )
import qualified Language.Sonic.Syntax.Sequence
                                               as Syn
                                                ( Sequence(..) )
import           Language.Sonic.Compiler.Context
                                                ( FileContext )
import           Language.Sonic.Compiler.Unique ( MonadUnique )
import           Language.Sonic.Compiler.Provenance
                                                ( WithProv(..)
                                                , Prov(..)
                                                )
import           Language.Sonic.Compiler.Report ( MonadReport(..) )
import qualified Language.Sonic.Compiler.Report
                                               as Report
                                                ( Severity(..) )
import           Language.Sonic.Compiler.Path   ( Name
                                                , newName
                                                , localPath
                                                )
import qualified Language.Sonic.Compiler.Path.Constant
                                               as C
                                                ( tupleCtor )
import           Language.Sonic.Compiler.IR.EntityKind
                                                ( Var )
import           Language.Sonic.Compiler.Desugar.Internal
                                                ( pattern DiscardLoc
                                                , pattern SpanLoc
                                                , discardLoc
                                                , withSourceProv
                                                , desugarMaybeWithProv
                                                , parsedAt
                                                , generated
                                                , foldMapM
                                                )
import qualified Language.Sonic.Compiler.Desugar.Report
                                               as Report
import           Language.Sonic.Compiler.Desugar.IR.Pass
                                                ( Desugar
                                                , passDesugar
                                                )

import qualified Language.Sonic.Syntax.Name    as Syn
                                                ( CtorName )
import qualified Language.Sonic.Syntax.Attribute
                                               as Syn
                                                ( WithAttrSet(..) )
import qualified Language.Sonic.Syntax.Declaration
                                               as Syn
                                                ( DataDecl(..)
                                                , ClassDecl(..)
                                                , InstanceDecl(..)
                                                , WhereClause(..)
                                                , SimpleDecl(..)
                                                , SignatureDecl(..)
                                                , ValueDecl(..)
                                                , FunctionDecl(..)
                                                , FunctionClause(..)
                                                )
import qualified Language.Sonic.Compiler.IR.Attribute
                                               as IR
                                                ( Attrs )
import qualified Language.Sonic.Compiler.IR.Type
                                               as IR
                                                ( Type )
import qualified Language.Sonic.Compiler.IR.Expression
                                               as IR
                                                ( Expr
                                                , Bind(..)
                                                , BindGroup(..)
                                                , CaseArm(..)
                                                )
import qualified Language.Sonic.Compiler.Desugar.IR.Expression
                                               as IR
import qualified Language.Sonic.Compiler.Desugar.IR.Pattern
                                               as IRPat
import qualified Language.Sonic.Compiler.IR.Declaration
                                               as IR
                                                ( DataDecl(..)
                                                , DataCtorDecl(..)
                                                , ClassDecl(..)
                                                , ClassMethodDecl(..)
                                                , InstanceDecl(..)
                                                )

import           Language.Sonic.Compiler.Desugar.Name
                                                ( desugarTyCtorName
                                                , desugarCtorName
                                                , desugarVarName
                                                , desugarClassName
                                                )
import           Language.Sonic.Compiler.Desugar.Path
                                                ( desugarPath )
import           Language.Sonic.Compiler.Desugar.Attribute
                                                ( desugarAttrSet )
import           Language.Sonic.Compiler.Desugar.Type
                                                ( desugarTyVarBinder
                                                , desugarType
                                                , desugarContext
                                                )
import           Language.Sonic.Compiler.Desugar.Pattern
                                                ( desugarPat )
import           Language.Sonic.Compiler.Desugar.Expression
                                                ( desugarExpr
                                                , desugarGuard
                                                , makePatBinds
                                                )

desugarDataDecl
  :: FileContext m
  => WithProv (IR.Attrs Desugar)
  -> Syn.DataDecl Syn.Position
  -> m (IR.DataDecl Desugar)
desugarDataDecl attrs Syn.DataDecl { Syn.dataName, Syn.vars = Syn.Sequence vars, Syn.ctors }
  = do
    dataName' <- withSourceProv (pure . desugarTyCtorName) dataName
    vars'     <- mapM (withSourceProv desugarTyVarBinder) vars
    ctors'    <- foldMapM (desugarDataCtorDecls . discardLoc) ctors
    pure IR.DataDecl { IR.attrs
                     , IR.name  = dataName'
                     , IR.vars  = vars'
                     , IR.ctors = ctors'
                     }
 where
  desugarDataCtorDecls (Syn.WhereClause (DiscardLoc (Syn.Sequence ds))) =
    foldMapM desugarDataCtorDeclWithLoc ds
  desugarDataCtorDeclWithLoc (SpanLoc declSpan decl) = do
    decls <- desugarDataCtorDecl decl
    prov  <- parsedAt declSpan
    pure $ addProvToDecls prov decls
  addProvToDecls prov [decl] = [WithProv prov decl]
  addProvToDecls prov xs     = map (WithProv (Derived passDesugar prov)) xs

desugarDataCtorDecl
  :: FileContext m
  => Syn.WithAttrSet (Syn.SignatureDecl Syn.CtorName) Syn.Position
  -> m [IR.DataCtorDecl Desugar]
desugarDataCtorDecl (Syn.WithAttrSet attrs (DiscardLoc Syn.SignatureDecl { Syn.names = DiscardLoc (Syn.Sequence names), Syn.type_ }))
  = do
    attrs' <- desugarMaybeWithProv (withSourceProv desugarAttrSet) attrs
    type'  <- withSourceProv desugarType type_
    mapM (desugarInnerWithName attrs' type') names
 where
  desugarInnerWithName attrs' type' name = do
    name' <- withSourceProv (pure . desugarCtorName) name
    pure IR.DataCtorDecl { IR.attrs = attrs'
                         , IR.name  = name'
                         , IR.type_ = type'
                         }

desugarClassDecl
  :: (FileContext m, MonadUnique m, MonadReport m)
  => WithProv (IR.Attrs Desugar)
  -> Syn.ClassDecl Syn.Position
  -> m (IR.ClassDecl Desugar)
desugarClassDecl attrs Syn.ClassDecl { Syn.context, Syn.className, Syn.vars = Syn.Sequence vars, Syn.methods }
  = do
    context' <- desugarMaybeWithProv (withSourceProv desugarContext) context
    className' <- withSourceProv (pure . desugarClassName) className
    vars' <- mapM (withSourceProv desugarTyVarBinder) vars
    (methods', defaultBinds) <- desugarClassMethods methods
    let defaultBinds' = generated $ IR.BindGroup defaultBinds
    pure IR.ClassDecl { attrs
                      , context      = context'
                      , name         = className'
                      , vars         = vars'
                      , methods      = methods'
                      , defaultBinds = [defaultBinds']
                      }
 where
  desugarClassMethods Nothing = pure ([], [])
  desugarClassMethods (Just (DiscardLoc (Syn.WhereClause (DiscardLoc (Syn.Sequence decls)))))
    = foldMapM (desugarClassMethodSimpleDeclWithAs . discardLoc) decls
  desugarClassMethodSimpleDeclWithAs (Syn.WithAttrSet declAs decl) = do
    declAs' <- desugarMaybeWithProv (withSourceProv desugarAttrSet) declAs
    decl'   <- desugarClassMethodSimpleDeclWithLoc declAs' decl
    pure $ destruct decl'
  destruct (Signatures sigs ) = (sigs, [])
  destruct (Bindings   binds) = ([], binds)

-- |
-- 'SimpleDecl' in 'ClassDecl' are desugared into one of the following:
--
-- 1. signature declarations: @['IR.ClassMethodDecl' 'Desugar']@
-- 2. binding declarations: @['IR.Bind' 'Desugar']@
--
-- The former comes from 'Syn.SignatureDecl', e.g. @a, b :: T@ → @a :: T@ and @b :: T@.
--
-- The latter comes from the other types of 'SimpleDecl's.
-- They're desugared into possibly mutiple binding declarations as well as 'SimpleDecl's in other places.
data DesugaredClassMethodSimpleDecl
  = Signatures [WithProv (IR.ClassMethodDecl Desugar)]
  | Bindings [WithProv (IR.Bind Desugar)]

desugarClassMethodSimpleDeclWithLoc
  :: (FileContext m, MonadUnique m, MonadReport m)
  => WithProv (IR.Attrs Desugar)
  -> Syn.Located Syn.Position Syn.SimpleDecl
  -> m DesugaredClassMethodSimpleDecl
desugarClassMethodSimpleDeclWithLoc attrs (SpanLoc declSpan (Syn.Value decl)) =
  do
    declProv <- parsedAt declSpan
    binds    <- desugarValueDecl attrs decl
    let binds' = map (WithProv (Derived passDesugar declProv)) binds
    pure $ Bindings binds'
desugarClassMethodSimpleDeclWithLoc attrs (SpanLoc declSpan (Syn.Function decl))
  = do
    declProv <- parsedAt declSpan
    bind     <- desugarFunctionDecl attrs decl
    pure $ Bindings [WithProv declProv bind]
desugarClassMethodSimpleDeclWithLoc attrs (SpanLoc declSpan (Syn.Signature Syn.SignatureDecl { Syn.names = DiscardLoc (Syn.Sequence names), Syn.type_ }))
  = desugarWithNames names
 where
  desugarWithNames [name] = do
    prov <- parsedAt declSpan
    decl <- desugarWithName name
    pure $ Signatures [WithProv prov decl]
  desugarWithNames xs = do
    prov  <- Derived passDesugar <$> parsedAt declSpan
    decls <- mapM desugarWithName xs
    pure . Signatures $ map (WithProv prov) decls
  desugarWithName name = do
    name' <- withSourceProv (pure . desugarVarName) name
    type' <- withSourceProv desugarType type_
    let decl = IR.ClassMethodDecl { attrs, name = name', type_ = type' }
    pure decl

desugarInstanceDecl
  :: (FileContext m, MonadUnique m, MonadReport m)
  => WithProv (IR.Attrs Desugar)
  -> Syn.InstanceDecl Syn.Position
  -> m (IR.InstanceDecl Desugar)
desugarInstanceDecl attrs Syn.InstanceDecl { Syn.context, Syn.className, Syn.types = Syn.Sequence types, Syn.methods }
  = do
    context'   <- desugarMaybeWithProv (withSourceProv desugarContext) context
    className' <- withSourceProv (pure . desugarPath desugarClassName) className
    types'     <- mapM (withSourceProv desugarType) types
    methods'   <- desugarInstanceMethods methods
    let bindGroup = IR.BindGroup methods'
    pure IR.InstanceDecl { attrs
                         , context = context'
                         , name    = className'
                         , types   = types'
                         , methods = [generated bindGroup]
                         }
 where
  desugarInstanceMethods Nothing = pure []
  desugarInstanceMethods (Just (DiscardLoc (Syn.WhereClause (DiscardLoc (Syn.Sequence declsWithAs)))))
    = do
      simpleDecls <- mapM (prepareSimpleDeclSource . discardLoc) declsWithAs
      desugarSimpleDecls simpleDecls

desugarValueDecl
  :: (FileContext m, MonadUnique m, MonadReport m)
  => WithProv (IR.Attrs Desugar)
  -> Syn.ValueDecl Syn.Position
  -> m [IR.Bind Desugar]
desugarValueDecl attrs Syn.ValueDecl { Syn.pat, Syn.body, Syn.bindings } = do
  pat'        <- withSourceProv desugarPat pat
  body'       <- withSourceProv desugarExpr body
  rhsTempName <- newName
  bind        <-
    IR.Bind attrs (generated rhsTempName) Nothing
      <$> desugarWhereBindings bindings body'
  pure (bind : makePatBinds attrs pat' rhsTempName)

desugarFunctionDecl
  :: (FileContext m, MonadUnique m, MonadReport m)
  => WithProv (IR.Attrs Desugar)
  -> Syn.FunctionDecl Syn.Position
  -> m (IR.Bind Desugar)
desugarFunctionDecl attrs Syn.FunctionDecl { Syn.name, Syn.clauses = SpanLoc clausesSpan (Syn.Sequence clauses) }
  = do
    name'       <- withSourceProv (pure . desugarVarName) name
    clausesProv <- parsedAt clausesSpan
    argCount    <- case clauses of
      [] -> do
        report Report.Error Report.EmptyFunctionClauses { Report.clausesProv }
        pure 1
      (DiscardLoc Syn.FunctionClause { Syn.pats = DiscardLoc (Syn.Sequence pats) } : _)
        -> pure (length pats)
    argNames <- replicateM argCount newName
    caseArms <- mapM (withSourceProv $ desugarFunctionClause argCount) clauses
    let caseArmsProv = Derived passDesugar clausesProv
    let caseExpr = IR.Case (generated (makeMatchTarget argNames))
                           (WithProv caseArmsProv caseArms)
    pure
      $ IR.Bind attrs name' Nothing (generated (makeLambda caseExpr argNames))
 where
  makeLambda = foldr (\x acc -> IR.Lambda (generated x) (generated acc))
  makeMatchTarget names = foldl
    (\acc x ->
      IR.Apply (generated acc) (generated (IR.Var (generated (localPath x))))
    )
    (tupleCtorExpr (length names))
    names
  tupleCtorExpr n = IR.Ctor (generated (C.tupleCtor n))

desugarFunctionClause
  :: (FileContext m, MonadUnique m, MonadReport m)
  => Int
  -> Syn.FunctionClause Syn.Position
  -> m (IR.CaseArm Desugar)
desugarFunctionClause argCount Syn.FunctionClause { Syn.pats = SpanLoc patsSpan (Syn.Sequence pats), Syn.guard, Syn.body, Syn.bindings }
  = do
    patsProv <- parsedAt patsSpan
    unless (length pats == argCount) $ report
      Report.Error
      Report.FunctionArgumentLengthMismatch { Report.patsProv }
    pats'  <- mapM (withSourceProv desugarPat) pats
    guard' <- mapM (withSourceProv desugarGuard) guard
    body'  <- withSourceProv desugarExpr body
    let caseArmPat = IRPat.Ctor (generated (C.tupleCtor argCount)) pats'
    caseArmBody <- desugarWhereBindings bindings body'
    pure $ IR.CaseArm (generated caseArmPat) guard' caseArmBody

desugarWhereBindings
  :: (FileContext m, MonadUnique m, MonadReport m)
  => Maybe (Syn.Located Syn.Position (Syn.WhereClause Syn.SimpleDecl))
  -> WithProv (IR.Expr Desugar)
  -> m (WithProv (IR.Expr Desugar))
desugarWhereBindings Nothing body = pure body
desugarWhereBindings (Just (DiscardLoc (Syn.WhereClause (DiscardLoc (Syn.Sequence []))))) body
  = pure body
desugarWhereBindings (Just (DiscardLoc (Syn.WhereClause (SpanLoc s (Syn.Sequence declsWithAs))))) body
  = do
    prov        <- Derived passDesugar <$> parsedAt s
    simpleDecls <- mapM (prepareSimpleDeclSource . discardLoc) declsWithAs
    binds       <- desugarSimpleDecls simpleDecls
    let bindGroup = IR.BindGroup binds
    pure . WithProv prov $ IR.Let [generated bindGroup] body
 where

data NameState
  = Sig Prov (IR.Attrs Desugar) (Name Var) (WithProv (IR.Type Desugar))
  | Bind (WithProv (IR.Bind Desugar))

-- |
-- We take 'SimpleDeclSource' type instead of @'Syn.WithAttrSet' 'Syn.SimpleDecl' 'Syn.Position'@ in 'desugarSimpleDecls'.
--
-- Sometimes 'Syn.SimpleDecl' data is deeply embeded in the structure (e.g. in 'Syn.Decl').
-- In that case, we have to build up a data for 'desugarSimpleDecls' manually after destructing the structure.
--
-- Since constructing 'Syn.WithAttrSet' (and especially 'Syn.Located') is unreasonably tedious,
-- we ended up with having new data structure 'SimpleDeclSource' and have callers build this instead.
data SimpleDeclSource
  = SimpleDeclSource
  { attrs    :: WithProv (IR.Attrs Desugar)
  , declProv :: Prov
  , decl     :: Syn.SimpleDecl Syn.Position
  }

prepareSimpleDeclSource
  :: FileContext m
  => Syn.WithAttrSet Syn.SimpleDecl Syn.Position
  -> m SimpleDeclSource
prepareSimpleDeclSource (Syn.WithAttrSet attrs (SpanLoc declSpan decl)) = do
  attrs'   <- desugarMaybeWithProv (withSourceProv desugarAttrSet) attrs
  declProv <- parsedAt declSpan
  pure SimpleDeclSource { attrs = attrs', declProv, decl }

-- | Desugar a set of 'Syn.SimpleDecl's while merging 'Syn.SignatureDecl' into corresponding value declarations.
desugarSimpleDecls
  :: (FileContext m, MonadUnique m, MonadReport m)
  => [SimpleDeclSource]
  -> m [WithProv (IR.Bind Desugar)]
desugarSimpleDecls decls = do
  bindsMap <- execStateT (mapM_ desugarSimpleDecl decls) Map.empty
  finalize bindsMap
 where
  finalize = foldMapM $ \case
    Sig prov _ name _ ->
      report Report.Error (Report.StandaloneTypeSignature prov name) $> mempty
    Bind bind -> pure [bind]
  desugarSimpleDecl (SimpleDeclSource (WithProv _ attrs) declProv (Syn.Signature Syn.SignatureDecl { Syn.names = DiscardLoc (Syn.Sequence names), Syn.type_ }))
    = do
      let names' = map (desugarVarName . discardLoc) names
      type' <- lift $ withSourceProv desugarType type_
      mapM_ (addType declProv attrs type') names'
  desugarSimpleDecl (SimpleDeclSource attrs declProv (Syn.Value decl)) = do
    binds <- lift $ desugarValueDecl attrs decl
    mapM_ (addBind (Derived passDesugar declProv)) binds
  desugarSimpleDecl (SimpleDeclSource attrs declProv (Syn.Function decl)) = do
    bind <- lift $ desugarFunctionDecl attrs decl
    addBind declProv bind
  addType prov attrs sigType name = do
    m <- get
    case Map.lookup name m of
      Just (Bind (WithProv prov2 IR.Bind { IR.type_ = Just _ })) ->
        lift $ report Report.Error err
        where err = Report.DuplicatedTypeSignature [prov, prov2] name
      Just (Bind bind) -> modify (Map.insert name (Bind bind'))
        where bind' = fmap (insertType attrs sigType) bind
      Just (Sig prov2 _ _ _) -> lift $ report Report.Error err
        where err = Report.DuplicatedTypeSignature [prov, prov2] name
      Nothing -> modify (Map.insert name (Sig prov attrs name sigType))
  addBind prov bind@IR.Bind { IR.type_, IR.id = WithProv _ name } = do
    m <- get
    case (Map.lookup name m, type_) of
      (Just (Bind (WithProv prov2 _)), _) -> do
        lift $ report Report.Error err
        modify (Map.insert name (Bind (WithProv prov bind)))
        where err = Report.DuplicatedBindings [prov, prov2] name
      (Just (Sig prov2 _ _ _), Just _) -> do
        lift $ report Report.Error err
        modify (Map.insert name (Bind (WithProv prov bind)))
        where err = Report.DuplicatedTypeSignature [prov, prov2] name
      (Just (Sig _ attrs _ sigType), Nothing) -> modify
        (Map.insert name (Bind bind'))
       where
        bind' =
          WithProv (Derived passDesugar prov) (insertType attrs sigType bind)
      (Nothing, _) -> modify (Map.insert name (Bind (WithProv prov bind)))
  insertType sigAttrs type_ IR.Bind { IR.attrs = WithProv attrsProv attrs, IR.id = bindID, IR.rhs }
    = IR.Bind
      { IR.attrs = WithProv (Derived passDesugar attrsProv) (attrs <> sigAttrs)
      , IR.id    = bindID
      , IR.type_ = Just type_
      , IR.rhs
      }
