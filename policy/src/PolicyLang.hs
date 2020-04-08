{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-} -- TODO: check if I need this
{-# LANGUAGE RankNTypes        #-}
module PolicyLang where
import qualified Data.Text as T
import qualified Data.Map.Lazy as M
import qualified Control.Monad.State.Lazy as S
import qualified Text.Parsec   as P
import           Data.Maybe (maybeToList,fromMaybe,fromJust)
import           Control.Applicative ((<$>),(*>))
import           Control.Monad (foldM, forM_)
import qualified Text.PrettyPrint.Annotated as PP
import           Control.Monad.Identity (runIdentity)
import           Data.Ratio(numerator,denominator)
-- import           Debug.Trace(trace)
import Parser

killAllExprs :: ArithExpr -> Maybe ArithExpr
killAllExprs AllExpr = Nothing
killAllExprs x = Just x

fixAllExprs :: TxnStmt -> Maybe TxnStmt
fixAllExprs (AssertStmt be) = AssertStmt <$> beTraverseArithExprs killAllExprs be
fixAllExprs (RequireSignatureStmt i) = return $ (RequireSignatureStmt i)
fixAllExprs (LocalStmt v tp ae) = LocalStmt v tp <$> (traverseArithExpr killAllExprs ae)
fixAllExprs (IssueStmt amt tp dst) = (\a -> IssueStmt a tp dst)
                                    <$> (traverseArithExpr killAllExprs amt)
fixAllExprs (TransferStmt amt src dst) = (\a -> TransferStmt a src dst) <$> do
  amt' <- traverseArithSubExpr killAllExprs amt
  return $ case amt' of
    AllExpr -> AmountField src
    _ -> amt'

freshname :: Integer -> M.Map T.Text a -> (T.Text,Integer)
freshname ix vars = head $ do
    incr <- [0..]
    ix' <- return $ ix+incr
    ix'' <- return $ ix'+1
    varname <- return $ T.pack $ "__fresh" ++ show ix'
    if (M.member varname vars) then [] else return (varname,ix'')

newvar :: Integer -> M.Map T.Text a -> ArithExpr -> (Integer,T.Text,[TxnStmt])
newvar ix vars expr = (ix',varname,[LocalStmt varname Nothing expr])
  where (varname,ix') = freshname ix vars

-- NOTE: assumes that vars-for-subexpressions transformation has been
-- done
addExprPreconds :: [TxnStmt] -> [TxnStmt]
addExprPreconds ls = do
  l <- ls
  case l of
    LocalStmt _ _ ae -> case ae of
      ConstAmountExpr i -> [AssertStmt (GeExpr (ConstAmountExpr i)
                                               (ConstAmountExpr 0)), l]
      ConstFractionExpr fr -> [AssertStmt (GeExpr (ConstFractionExpr fr)
                                                  (ConstFractionExpr 0)), l]
      MinusExpr le re -> [AssertStmt (GeExpr le re), l]
      -- NOTE: assumes that all other expressions are non-negative if
      -- their inputs are non-negative
      _ -> [l]
    _ -> return l

unifyTypes :: DataType -> DataType -> Maybe DataType
unifyTypes x y | x == y = Just x
unifyTypes AmountType FractionType = Just FractionType
unifyTypes FractionType AmountType = Just FractionType
unifyTypes _ _ = Nothing

-- TODO: Identity/AssetTypeType arithmetic??!
typeForExpr :: M.Map T.Text DataType -> ArithExpr -> Maybe DataType
typeForExpr _ (ConstAmountExpr _) = Just AmountType
typeForExpr _ (ConstFractionExpr _) = Just FractionType
typeForExpr _ (AmountField _) = Just AmountType
typeForExpr _ (OwnerField _) = Just IdentityType
typeForExpr tpMap (ArithVar v) = M.lookup v tpMap
typeForExpr tpMap (PlusExpr l r) = do
  lt <- typeForExpr tpMap l
  rt <- typeForExpr tpMap r
  unifyTypes lt rt
typeForExpr tpMap (TimesExpr l r) = do
  lt <- typeForExpr tpMap l
  rt <- typeForExpr tpMap r
  unifyTypes lt rt
typeForExpr tpMap (MinusExpr l r) = do
  lt <- typeForExpr tpMap l
  rt <- typeForExpr tpMap r
  case (lt,rt) of
    (AmountType,AmountType) -> Just AmountType
    _ -> Nothing
typeForExpr tpMap (RoundExpr e) = do
  et <- typeForExpr tpMap e
  -- check that et can unify with FractionType
  _ <- unifyTypes FractionType et
  return AmountType
typeForExpr _ _ = Nothing

typecheckBoolExpr :: M.Map T.Text DataType -> BoolExpr -> Maybe ()
typecheckBoolExpr _ TrueExpr = Just ()
typecheckBoolExpr _ FalseExpr = Just ()
typecheckBoolExpr tpMap (NotExpr e) = typecheckBoolExpr tpMap e
typecheckBoolExpr tpMap (AndExpr l r)
  = typecheckBoolExpr tpMap l *> typecheckBoolExpr tpMap r
typecheckBoolExpr tpMap (OrExpr l r)
  = typecheckBoolExpr tpMap l *> typecheckBoolExpr tpMap r
typecheckBoolExpr tpMap (EqExpr l r) = do
  lt <- typeForExpr tpMap l
  rt <- typeForExpr tpMap r
  if lt == rt then return () else Nothing
typecheckBoolExpr tpMap (GtExpr l r) = do
  lt <- typeForExpr tpMap l
  rt <- typeForExpr tpMap r
  case (lt,rt) of
    (AmountType,AmountType) -> Just ()
    (FractionType,FractionType) -> Just ()
    _ -> Nothing
typecheckBoolExpr tpMap (GeExpr l r) = do
  lt <- typeForExpr tpMap l
  rt <- typeForExpr tpMap r
  case (lt,rt) of
    (AmountType,AmountType) -> Just ()
    (FractionType,FractionType) -> Just ()
    _ -> Nothing
typecheckBoolExpr tpMap (LeExpr l r) = do
  lt <- typeForExpr tpMap l
  rt <- typeForExpr tpMap r
  case (lt,rt) of
    (AmountType,AmountType) -> Just ()
    (FractionType,FractionType) -> Just ()
    _ -> Nothing
typecheckBoolExpr tpMap (LtExpr l r) = do
  lt <- typeForExpr tpMap l
  rt <- typeForExpr tpMap r
  case (lt,rt) of
    (AmountType,AmountType) -> Just ()
    (FractionType,FractionType) -> Just ()
    _ -> Nothing

typecheckTxnDecl :: M.Map T.Text DataType -> TxnDecl -> TxnDecl
typecheckTxnDecl globalTps txn@(TxnDecl{ _txnBody = body })
  = txn {
    _txnBody = go globalTps body
  }
  where
    go _ [] = []
    go tpMap (l:ls) = l' : go tpMap' ls
      where
        (tpMap',l') = case res of
          Just x -> x
          _ -> error $ "Bad typechecking stmt '" ++ show l ++ "' (dict '" ++ show tpMap ++ "')"
        res = case l of
          AssertStmt be -> do
            typecheckBoolExpr tpMap be
            return (tpMap,l)
          RequireSignatureStmt v -> do
            IdentityType <- M.lookup v tpMap
            return (tpMap,l)
          LocalStmt v Nothing ae -> do
            ae_tp <- typeForExpr tpMap ae
            return (M.insert v ae_tp tpMap,LocalStmt v (Just ae_tp) ae)
          LocalStmt v (Just tp) ae -> do
            ae_tp <- typeForExpr tpMap ae
            _ <- unifyTypes tp ae_tp
            return (M.insert v tp tpMap,l)
          IssueStmt amt _ _ -> do
            amt_tp <- typeForExpr tpMap amt
            _ <- unifyTypes AmountType amt_tp
            return (tpMap,l)
          TransferStmt amt _ _ -> do
            amt_tp <- typeForExpr tpMap amt
            _ <- unifyTypes AmountType amt_tp
            return (tpMap,l)

splitInouts :: TxnDecl -> TxnDecl
splitInouts txn@(TxnDecl{ _txnParams = params, _txnBody = body })
  = txn {
    _txnParams = newParams,
    _txnBody = map (update inRecs outRecs) (go inRecs outRecs body) ++ inConsumed
  }
  where
    inParams = filter _txnparamIn params
    outParams = filter _txnparamOut params
    inRecs  = M.fromList $ [(_txnparamName p,(_txnparamName p <> "_in"))  | p <- inParams]
    outRecs = M.fromList $ [(_txnparamName p,(_txnparamName p <> "_out")) | p <- outParams]

    newParams = [p { _txnparamName = (fromJust $ M.lookup (_txnparamName p) inRecs),
                     _txnparamOut = False } | p <- inParams]
              ++[p { _txnparamName = (fromJust $ M.lookup (_txnparamName p) outRecs),
                     _txnparamIn  = False } | p <- outParams]

    update inMap _ (TransferStmt ae src Nothing)
      = TransferStmt ae (fromJust $ M.lookup src inMap) Nothing
    update inMap outMap (TransferStmt ae src (Just dst))
      = TransferStmt ae (fromJust $ M.lookup src inMap)
                        (Just $ fromJust $ M.lookup dst outMap)
    update _ outMap (IssueStmt ae tp dst)
      = IssueStmt ae tp (fromJust $ M.lookup dst outMap)
    update _ _ x = x

    go _ _ [] = []
    go recs oRecs (l:ls) = l' : go recs' oRecs ls
      where
        recs' = case l of
          TransferStmt _ _ (Just v) -> M.insert v (fromJust $ M.lookup v oRecs) recs
          _ -> recs
        l' = case l of
          AssertStmt be
            -> AssertStmt $ over beTraverseArithExprs  (replaceResource) be
          LocalStmt v tp ae
            -> LocalStmt v tp $ replaceResource ae
          IssueStmt ae tp dst
            -> IssueStmt (replaceResource ae) tp dst
          TransferStmt ae src dst
            -> TransferStmt (replaceResource ae) src dst
          RequireSignatureStmt _ -> l
        replaceResource = over traverseArithExpr $ \x ->
          case x of
            AmountField v -> AmountField $ case (M.lookup v recs) of
              Nothing -> fromJust $ M.lookup v oRecs
              Just val -> val
            OwnerField v  -> OwnerField  $ case (M.lookup v recs) of
              Nothing -> fromJust $ M.lookup v oRecs
              Just val -> val
            _ -> over traverseArithSubExpr (replaceResource) x

    inConsumed = flip map (snd <$> M.toList inRecs) $ \x ->
      AssertStmt $ EqExpr (AmountField x) (ConstAmountExpr 0)

replaceVars :: M.Map T.Text ArithExpr -> ArithExpr -> ArithExpr
replaceVars m = over traverseArithExpr $ \x -> case x of
  ArithVar v -> fromMaybe x $ M.lookup v m
  _ -> x

replaceBeVars :: M.Map T.Text ArithExpr -> BoolExpr -> BoolExpr
replaceBeVars m = over beTraverseArithExprs $ replaceVars m

linLookup :: Eq t => t -> [(t, a)] -> Maybe a
linLookup _ [] = Nothing
linLookup x ((k,v):_) | x == k = Just v
linLookup x (_:vs) = linLookup x vs

deleteRedundantStmts :: [TxnStmt] -> [TxnStmt]
deleteRedundantStmts = go [] [] (M.fromList [])
  where
    go _ _ _ [] = []
    go knownExprs knownAsserts replacedVars (l:ls)
      = maybeToList l' ++ go knownExprs' knownAsserts' replacedVars' ls
      where
        (l',knownExprs',knownAsserts',replacedVars') = case l of
          AssertStmt be -> let be' = (replaceBeVars replacedVars be) in
            if be' `elem` knownAsserts
              then (Nothing,knownExprs,knownAsserts,replacedVars)
              else (Just $ AssertStmt be',knownExprs,be':knownAsserts,replacedVars)
          LocalStmt v tp ae -> let ae' = (replaceVars replacedVars ae) in
            case (linLookup (ae',tp) knownExprs) of
              Nothing -> (Just $ LocalStmt v tp ae', ((ArithVar v,tp),v):((ae',tp),v):knownExprs,
                          knownAsserts, replacedVars)
              Just v' -> (Nothing, knownExprs, knownAsserts,
                          M.insert v (ArithVar v') replacedVars)
          IssueStmt ae tp dst
            -> (Just $ IssueStmt (replaceVars replacedVars ae) tp dst,
                knownExprs,knownAsserts,replacedVars)
          TransferStmt ae src dst
            -> (Just $ TransferStmt (replaceVars replacedVars ae) src dst,
                knownExprs,knownAsserts,replacedVars)
          _ -> (Just l,knownExprs,knownAsserts,replacedVars)

explicitAmounts :: [TxnStmt] -> [TxnStmt]
explicitAmounts body = go body
  where
    go ls = ls'
      where
        (ls',_) = flip S.runState (M.fromList []) $ goStmt ls

    goStmt :: [TxnStmt] -> S.State (M.Map T.Text ArithExpr) [TxnStmt]
    goStmt [] = return []
    goStmt (l:ls) = do
      l' <- case l of
        AssertStmt be -> AssertStmt <$> beTraverseArithExprs goAE be
        LocalStmt v tp ae -> LocalStmt v tp <$> goAE ae
        IssueStmt ae tp dst -> do
          ae' <- goAE ae
          updates <- S.get
          updates' <- return $ M.insert dst ae' updates
          S.put updates'
          return $ IssueStmt ae' tp dst
        TransferStmt ae src dst -> do
          ae' <- goAE ae
          updates <- S.get
          oldSrcVal <- return $ fromMaybe (OldExpr (AmountField src))
                              $ M.lookup src updates
          updates' <- return $ M.insert src (MinusExpr oldSrcVal ae') updates
          updates'' <- return $ case dst of
            Nothing -> updates'
            Just dst' -> M.insert dst' ae' updates'
          S.put updates''
          return $ TransferStmt ae' src dst
        _ -> return l
      (l':) <$> goStmt ls

    goAE :: ArithExpr -> S.State (M.Map T.Text ArithExpr) ArithExpr
    goAE ae = flip traverseNonOldArithExpr ae $ \e -> do
      case e of
        AmountField v -> do
          updates <- S.get
          return $ fromMaybe e $ M.lookup v updates
        _ -> return e

sortTxnStmts :: [TxnStmt] -> [TxnStmt]
sortTxnStmts body = let (locals,nonops,ops) = go body
                  in locals ++ nonops ++ ops
  where
    go [] = ([],[],[])
    go (l:ls) = let (locals,nonops,ops) = go ls in
      case l of
        (LocalStmt _ _ _) -> (l:locals,nonops,ops)
        (AssertStmt _) -> (locals,l:nonops,ops)
        (RequireSignatureStmt _) -> (locals,l:nonops,ops)
        (IssueStmt _ _ _) -> (locals,nonops,l:ops)
        (TransferStmt _ _ _) -> (locals,nonops,l:ops)

moveOldExprs :: [TxnStmt] -> [TxnStmt]
moveOldExprs body = go body
  where
    freshvar :: ArithExpr -> S.State (Integer,M.Map T.Text ArithExpr,[TxnStmt]) (T.Text,[TxnStmt])
    freshvar expr = do
      (ix,vars,stmts) <- S.get
      (ix',varname,newstmts) <- return $ newvar ix vars expr
      vars' <- return $ M.insert varname expr vars
      S.put (ix',vars',stmts)
      return (varname,newstmts)

    go ls = oldDecls ++ ls'
      where
        (ls',(_,_,oldDecls)) = flip S.runState (0,M.fromList $ freeVars body,[]) $ goStmt ls

    goStmt [] = return []
    goStmt (l:ls) = do
      l' <- case l of
        AssertStmt be -> AssertStmt <$> beTraverseArithExprs goAE be
        LocalStmt v tp ae -> LocalStmt v tp <$> goAE ae
        IssueStmt ae tp dst
          -> (\ae' -> IssueStmt ae' tp dst) <$> goAE ae
        TransferStmt ae src dst
          -> (\ae' -> TransferStmt ae' src dst) <$> goAE ae
        _ -> return l
      (l':) <$> goStmt ls

    goAE ae = do
      ae' <- traverseArithSubExpr goAE ae
      case ae' of
        OldExpr e -> do
          (varname,oldDecl) <- freshvar e
          (ix,vars,oldDecls) <- S.get
          S.put (ix,vars,oldDecls++oldDecl)
          return $ ArithVar varname
        _ -> return ae'

explicitSubExprs :: [TxnStmt] -> [TxnStmt]
explicitSubExprs body = fst $ flip S.runState (0,(M.fromList $ freeVars body) :: M.Map T.Text ArithExpr) $ goStmt body
  where
    freshvar :: ArithExpr -> S.State (Integer,M.Map T.Text ArithExpr) (T.Text,[TxnStmt])
    freshvar expr = do
      (ix,vars) <- S.get
      (ix',varname,newstmts) <- return $ newvar ix vars expr
      vars' <- return $ M.insert varname expr vars
      S.put (ix',vars')
      return (varname,newstmts)

    goStmt :: [TxnStmt] -> S.State (Integer,M.Map T.Text ArithExpr) [TxnStmt]
    goStmt [] = return []
    goStmt (l:ls) = do
      l' <- case l of
        AssertStmt be -> do
          (vstmts,be') <- goBE be
          return $ vstmts ++ [AssertStmt be']
        LocalStmt v tp ae -> do
          (vstmts,ae') <- goAE ae
          (ix,vars) <- S.get
          vars' <- return $ M.insert v ae' vars
          S.put (ix,vars')
          return $ vstmts ++ [LocalStmt v tp ae']
        IssueStmt ae tp dst -> do
          (vstmts,ae') <- goAE ae
          (varname,newstmts) <- freshvar ae'
          return $ vstmts ++ newstmts ++ [IssueStmt (ArithVar varname) tp dst]
        TransferStmt ae src dst -> do
          (vstmts,ae') <- goAE ae
          (varname,newstmts) <- freshvar ae'
          return $ vstmts ++ newstmts ++ [TransferStmt (ArithVar varname) src dst]
        _ -> return [l]
      ls' <- goStmt ls
      return $ l'++ls'
    goBE :: BoolExpr -> S.State (Integer,M.Map T.Text ArithExpr) ([TxnStmt],BoolExpr)
    goBE be = do
      (ix,vars) <- S.get
      (be',(ix',vars',vstmts)) <- return $ flip S.runState (ix,vars,[]) $ do
        be' <- beTraverseArithExprs goAEinner be
        flip beTraverseArithExprs be' $ \ae -> do
          (ix',vars',vstmts) <- S.get
          (ix'',varname,newstmts) <- return $ newvar ix' vars' ae
          vars'' <- return $ M.insert varname ae vars'
          vstmts' <- return $ vstmts ++ newstmts
          S.put (ix'',vars'',vstmts')
          return $ ArithVar varname
      S.put (ix',vars')
      return (vstmts,be')

    goAE :: ArithExpr -> S.State (Integer,M.Map T.Text ArithExpr) ([TxnStmt],ArithExpr)
    goAE ae = do
      (ix,vars) <- S.get
      let (ae',(ix',vars',vstmts)) = flip S.runState (ix,vars,[]) $ goAEinner ae
      S.put (ix',vars')
      return (vstmts,ae')

    goAEinner :: ArithExpr -> S.State (Integer,M.Map T.Text ArithExpr,[TxnStmt]) ArithExpr
    goAEinner expr@(ConstAmountExpr _) = do
      (ix,vars,vstmts) <- S.get
      (ix',varname,newstmts) <- return $ newvar ix vars expr
      vars' <- return $ M.insert varname expr vars
      vstmts' <- return $ vstmts ++ newstmts
      S.put (ix',vars',vstmts')
      return $ ArithVar varname
    goAEinner expr@(ConstFractionExpr _) = do
      (ix,vars,vstmts) <- S.get
      (ix',varname,newstmts) <- return $ newvar ix vars expr
      vars' <- return $ M.insert varname expr vars
      vstmts' <- return $ vstmts ++ newstmts
      S.put (ix',vars',vstmts')
      return $ ArithVar varname
    goAEinner ae = flip traverseArithSubExpr ae $ \expr -> do
      expr' <- goAEinner expr
      (ix,vars,vstmts) <- S.get
      (ix',varname,newstmts) <- return $ newvar ix vars expr'
      vars' <- return $ M.insert varname expr' vars
      vstmts' <- return $ vstmts ++ newstmts
      S.put (ix',vars',vstmts')
      return $ ArithVar varname

-- explicitSubExprsTxnDecl (TxnDecl name params requires ensures body)
--   = TxnDecl name params requires ensures $ explicitSubExprs body

over :: (forall f. Applicative f => (a1 -> f a2) -> a3 -> f c)
                            -> (a1 -> a2) -> a3 -> c
over l f = runIdentity . l (pure . f)

explicitSubExprsTxnDecl :: TxnDecl -> TxnDecl
explicitSubExprsTxnDecl = over txnBody explicitSubExprs

explicitReqEns :: TxnDecl -> TxnDecl
explicitReqEns (TxnDecl name params requires ensures body)
  = TxnDecl name params [] [] (reqAsserts ++ body ++ ensAsserts)
  where
    reqAsserts = map AssertStmt requires
    ensAsserts = map AssertStmt ensures

data PolicyFileWithInit = PolicyFileWithInit
  { _polfwiInitChecks :: [BoolExpr]
  , _polfwiPolf :: PolicyFile
  }
  deriving (Eq,Show,Read)

explicitGParamInit :: PolicyFile -> PolicyFile
explicitGParamInit ast = ast'
  where
    ast' = ast {
      _polfGParams = flip map (_polfGParams ast) $ (\gpd -> gpd { _gparamInvs = [] }),
      _polfTxns = init_txn : _polfTxns ast
    }

    init_txn = TxnDecl
      { _txnName = "init_txn", _txnParams = [], _txnRequires = []
      , _txnEnsures = do
          var <- _polfGParams ast
          vname <- return $ _gparamName var
          typeInv <- return $ case _gparamType var of
            FractionType -> [GeExpr (ArithVar vname) (ConstFractionExpr 0)]
            AmountType -> [GeExpr (ArithVar vname) (ConstAmountExpr 0)]
            _ -> []
          inv <- typeInv++_gparamInvs var
          return inv
      , _txnBody = [] }

typecheck :: PolicyFile -> PolicyFile
typecheck ast = over (polfTxns) (map $ typecheckTxnDecl globalTypes) ast
  where
    globalTypes = M.insert (_batsIssuer $ _polfBats ast) IdentityType
                $ M.insert (_batsType   $ _polfBats ast) ResourceTypeType
                $ M.fromList
                $ (\decl -> (_gparamName decl, _gparamType decl))
                  <$> _polfGParams ast

newtype PSIdVar = PSIdVar Int deriving (Eq,Show,Read)
newtype PSAmtVar = PSAmtVar Int deriving (Eq,Show,Read)
newtype PSFracVar = PSFracVar Int deriving (Eq,Show,Read)
newtype PSResTypeVar = PSResTypeVar Int deriving (Eq,Show,Read)
newtype PSResVar = PSResVar Int deriving (Eq,Show,Read)
newtype PSBoolVar = PSBoolVar Int deriving (Eq,Show,Read)

data PSIdOp = PSOwnerOfOp PSResVar
            | PSIdVarOp PSIdVar
 deriving (Eq,Show,Read)

data PSResTypeOp = PSTypeOfOp PSResVar
                 | PSResTypeVarOp PSResTypeVar
 deriving (Eq,Show,Read)

data PSAmtOp
  = PSAmtVarOp PSAmtVar
  | PSAmtConstOp Int
  | PSAmtOfResOp PSResVar
  | PSAmtPlusOp PSAmtVar PSAmtVar
  | PSAmtMinusOp PSAmtVar PSAmtVar
  | PSAmtTimesOp PSAmtVar PSAmtVar
  | PSAmtRoundOp PSFracVar
 deriving (Eq,Show,Read)

data PSFracOp
  = PSFracVarOp PSFracVar
  | PSFracConstOp Rational
  | PSFracPlusOp PSFracVar PSFracVar
  | PSFracTimesOp PSFracVar PSFracVar
  | PSFracTimesAmtOp PSFracVar PSAmtVar
  | PSFracAmtTimesOp PSAmtVar PSFracVar
 deriving (Eq,Show,Read)

data PSBoolOp
  = PSBoolConstOp Bool

  | PSIdEq PSIdVar PSIdVar
  | PSAmtEq PSAmtVar PSAmtVar
  | PSFracEq PSFracVar PSFracVar
  | PSResTypeEq PSResTypeVar PSResTypeVar

  | PSNot PSBoolVar
  | PSAnd PSBoolVar PSBoolVar
  | PSOr PSBoolVar PSBoolVar

  | PSAmtGe PSAmtVar PSAmtVar
  | PSFracGe PSFracVar PSFracVar
  | PSFracAmtGe PSFracVar PSAmtVar
  | PSAmtFracGe PSAmtVar PSFracVar
 deriving (Eq,Show,Read)

data PSTxnOp
  = PSIssue PSAmtVar PSResTypeVar PSResVar
  | PSTransfer PSAmtVar PSResVar (Maybe PSResVar)
  deriving (Eq,Show,Read)



data PolicyTxnCheck = PolicyTxnCheck
  { _ptcTxnName      :: T.Text

  , _ptcInParams  :: [PSResTypeVar]
  , _ptcOutParams :: [PSResTypeVar]

  , _ptcIdOps        :: [PSIdOp]
  , _ptcRtOps        :: [PSResTypeOp]
  , _ptcAmtOps       :: [PSAmtOp]
  , _ptcFracOps      :: [PSFracOp]

  , _ptcBoolOps      :: [PSBoolOp]

  , _ptcAssertions   :: [PSBoolVar]
  , _ptcSignatures   :: [PSIdVar]
  , _ptcTxnTemplate  :: [PSTxnOp]
  } deriving (Eq,Show,Read)

data PolicyScript = PolicyScript
  { _polNumIdGlobals      :: Int
  , _polNumResTypeGlobals :: Int
  , _polNumAmtGlobals     :: Int
  , _polNumFracGlobals    :: Int
  , _polInitCheck         :: PolicyTxnCheck
  , _polTxns              :: [PolicyTxnCheck]
  } deriving (Eq,Show,Read)

data PolicyTxnConversionState = PolicyTxnConversionState
  { _ptcTypes :: M.Map T.Text DataType

  , _ptcResVars :: M.Map T.Text PSResVar
  , _ptcIdVars :: M.Map T.Text PSIdVar
  , _ptcResTypeVars :: M.Map T.Text PSResTypeVar
  , _ptcAmtVars :: M.Map T.Text PSAmtVar
  , _ptcFracVars :: M.Map T.Text PSFracVar

  , _ptcTxnSoFar :: PolicyTxnCheck
  } deriving (Eq,Show,Read)

convertPolfToScript :: PolicyFile -> PolicyScript
convertPolfToScript (PolicyFile { _polfBats = _
                                , _polfGParams = _
                                , _polfTxns = [] }) = undefined
convertPolfToScript (PolicyFile { _polfBats = bats
                                , _polfGParams = gparams
                                , _polfTxns = (init_txn:txns) })
  = final_script
  where
    final_script = PolicyScript { _polNumIdGlobals = M.size id_globals
                 , _polNumResTypeGlobals = M.size res_type_globals
                 , _polNumAmtGlobals = M.size amt_globals
                 , _polNumFracGlobals = M.size frac_globals
                 , _polInitCheck = txnConvert init_txn
                 , _polTxns = map txnConvert txns
                 }
    id_globals = M.fromList
      $ [(_batsIssuer bats,PSIdVar 0)]
        ++ ((\ (i,gparam) -> (_gparamName gparam, PSIdVar i))
            <$> zip [1..] (filter ((== IdentityType) . _gparamType) gparams))
    res_type_globals = M.fromList
      $ [(_batsType bats,PSResTypeVar 0)]
        ++ ((\ (i,gparam) -> (_gparamName gparam, PSResTypeVar i))
            <$> zip [1..] (filter ((== ResourceTypeType) . _gparamType) gparams))
    amt_globals = M.fromList
      $ (\ (i,gparam) -> (_gparamName gparam, PSAmtVar i))
      <$> zip [0..] (filter ((== AmountType) . _gparamType) gparams)
    frac_globals = M.fromList
      $ (\ (i,gparam) -> (_gparamName gparam, PSFracVar i))
      <$> zip [0..] (filter ((== FractionType) . _gparamType) gparams)

    txnConvert (TxnDecl { _txnRequires = (_:_) }) = undefined
    txnConvert (TxnDecl { _txnEnsures  = (_:_) }) = undefined
    txnConvert (TxnDecl { _txnName = txnName, _txnParams = params
                        , _txnRequires = [], _txnEnsures = []
                        , _txnBody = bodyStmts })
      = _ptcTxnSoFar $ snd $ flip S.runState (PolicyTxnConversionState
                        { _ptcResVars = M.fromList
                            $ (\ (i,param) -> (_txnparamName param,PSResVar i))
                            <$> zip [0..] params
                        , _ptcIdVars = id_globals
                        , _ptcResTypeVars = res_type_globals
                        , _ptcAmtVars = amt_globals
                        , _ptcFracVars = frac_globals
                        , _ptcTypes = M.unions
                            [ const FractionType <$> frac_globals
                            , const AmountType <$> amt_globals
                            , const IdentityType <$> id_globals
                            , const ResourceTypeType <$> res_type_globals
                            ]
                        , _ptcTxnSoFar = PolicyTxnCheck
                            { _ptcTxnName = txnName
                            , _ptcInParams = map (\t -> fromJust $ M.lookup (_txnparamType t) res_type_globals) $ filter _txnparamIn params
                            , _ptcOutParams = map (\t -> fromJust $ M.lookup (_txnparamType t) res_type_globals) $ filter _txnparamOut params
                            , _ptcIdOps = []
                            , _ptcRtOps = []
                            , _ptcAmtOps = []
                            , _ptcFracOps = []
                            , _ptcBoolOps = []
                            , _ptcAssertions = []
                            , _ptcSignatures = []
                            , _ptcTxnTemplate = []
                            }
                        }) $ forM_ bodyStmts convertStmt

    -- TODO: for the love of god refactor this
    convertStmt (IssueStmt ae tp dst) = do
      cx_state@(PolicyTxnConversionState { _ptcTxnSoFar = txn }) <- S.get
      let (ArithVar av) = ae;

      amt_var <- return $ fromJust $ M.lookup av  $ _ptcAmtVars cx_state
      restype <- return $ fromJust $ M.lookup tp  $ _ptcResTypeVars cx_state
      dst_res <- return $ fromJust $ M.lookup dst $ _ptcResVars cx_state

      S.put $ (cx_state {
        _ptcTxnSoFar = txn {
          _ptcTxnTemplate = (_ptcTxnTemplate txn
                            ++ [PSIssue amt_var restype dst_res])
          }
        })
    convertStmt (TransferStmt ae src dst) = do
      cx_state@(PolicyTxnConversionState { _ptcTxnSoFar = txn }) <- S.get
      let (ArithVar av) = ae;

      let { (Just amt_var) = M.lookup av  $ _ptcAmtVars cx_state }
      let { (Just src_res) = M.lookup src $ _ptcResVars cx_state }
      let { (Just dst_res) = case dst of
        Nothing -> Just Nothing
        Just dst' -> fmap Just $ M.lookup dst' $ _ptcResVars cx_state
      }

      S.put $ cx_state {
        _ptcTxnSoFar = txn {
          _ptcTxnTemplate = (_ptcTxnTemplate txn
                            ++ [PSTransfer amt_var src_res dst_res])
          }
        }

    convertStmt (RequireSignatureStmt id_name) = do
      cx_state@(PolicyTxnConversionState { _ptcTxnSoFar = txn }) <- S.get
      let (Just id_var) = M.lookup id_name $ _ptcIdVars cx_state

      S.put $ cx_state {
        _ptcTxnSoFar = txn {
          _ptcSignatures = (_ptcSignatures txn ++ [id_var])
          }
        }

    convertStmt (LocalStmt var tp0 expr) = do
      (tp) <- return $ fromJust tp0
      cx_state@(PolicyTxnConversionState { _ptcTxnSoFar = txn }) <- S.get
      (case (M.lookup var $ _ptcTypes cx_state) of
        Nothing -> return ()
        _ -> undefined)

      let types' = M.insert var tp $ _ptcTypes cx_state

      case expr of
        OldExpr _ -> undefined
        AllExpr -> undefined
        ConstAmountExpr n -> do
          let ix = _polNumAmtGlobals final_script + length (_ptcAmtOps txn)

          S.put $ cx_state {
            _ptcTypes = types',
            _ptcAmtVars = M.insert var (PSAmtVar ix) $ _ptcAmtVars cx_state,
            _ptcTxnSoFar = txn {
              _ptcAmtOps = _ptcAmtOps txn ++ [PSAmtConstOp (fromInteger n)]
            }
          }
        ConstFractionExpr fv -> do
          let ix = _polNumFracGlobals final_script + length (_ptcFracOps txn)

          S.put $ cx_state {
            _ptcTypes = types',
            _ptcFracVars = M.insert var (PSFracVar ix) $ _ptcFracVars cx_state,
            _ptcTxnSoFar = txn {
              _ptcFracOps = _ptcFracOps txn ++ [PSFracConstOp fv]
            }
          }

        PlusExpr l r -> case tp of
          AmountType -> do
            let (ArithVar li) = l
            let (Just lv) = M.lookup li $ _ptcAmtVars cx_state
            let (ArithVar ri) = r
            let (Just rv) = M.lookup ri $ _ptcAmtVars cx_state

            let ix = _polNumAmtGlobals final_script + length (_ptcAmtOps txn)

            S.put $ cx_state {
              _ptcTypes = types',
              _ptcAmtVars = M.insert var (PSAmtVar ix) $ _ptcAmtVars cx_state,
              _ptcTxnSoFar = txn {
                _ptcAmtOps = _ptcAmtOps txn ++ [PSAmtPlusOp lv rv]
              }
            }
          FractionType -> do
            let (ArithVar li) = l
            let (Just lv) = M.lookup li $ _ptcFracVars cx_state
            let (ArithVar ri) = r
            let (Just rv) = M.lookup ri $ _ptcFracVars cx_state

            let ix = _polNumFracGlobals final_script + length (_ptcFracOps txn)

            S.put $ cx_state {
              _ptcTypes = types',
              _ptcFracVars = M.insert var (PSFracVar ix) $ _ptcFracVars cx_state,
              _ptcTxnSoFar = txn {
                _ptcFracOps = _ptcFracOps txn ++ [PSFracPlusOp lv rv]
              }
            }
          _ -> undefined

        TimesExpr l r -> case tp of
          AmountType -> do
            let (ArithVar li) = l
            let (Just lv) = M.lookup li $ _ptcAmtVars cx_state
            let (ArithVar ri) = r
            let (Just rv) = M.lookup ri $ _ptcAmtVars cx_state

            let ix = _polNumAmtGlobals final_script + length (_ptcAmtOps txn)

            S.put $ cx_state {
              _ptcTypes = types',
              _ptcAmtVars = M.insert var (PSAmtVar ix) $ _ptcAmtVars cx_state,
              _ptcTxnSoFar = txn {
                _ptcAmtOps = _ptcAmtOps txn ++ [PSAmtTimesOp lv rv]
              }
            }
          FractionType -> do
            let ix = _polNumFracGlobals final_script + length (_ptcFracOps txn)
            let (ArithVar lv0) = l
            let (ArithVar rv0) = r
            case (M.lookup lv0 $ _ptcTypes cx_state, M.lookup rv0 $ _ptcTypes cx_state) of
              (Just FractionType,Just FractionType) -> do
                lv <- return $ fromJust $ M.lookup lv0 $ _ptcFracVars cx_state
                rv <- return $ fromJust $ M.lookup rv0 $ _ptcFracVars cx_state

                S.put $ cx_state {
                  _ptcTypes = types',
                  _ptcFracVars = M.insert var (PSFracVar ix) $ _ptcFracVars cx_state,
                  _ptcTxnSoFar = txn {
                    _ptcFracOps = _ptcFracOps txn ++ [PSFracTimesOp lv rv]
                  }
                }

              (Just FractionType,Just AmountType) -> do
                lv <- return $ fromJust $ M.lookup lv0 $ _ptcFracVars cx_state
                rv <- return $ fromJust $ M.lookup rv0 $ _ptcAmtVars cx_state

                S.put $ cx_state {
                  _ptcTypes = types',
                  _ptcFracVars = M.insert var (PSFracVar ix) $ _ptcFracVars cx_state,
                  _ptcTxnSoFar = txn {
                    _ptcFracOps = _ptcFracOps txn ++ [PSFracTimesAmtOp lv rv]
                  }
                }

              (Just AmountType,Just FractionType) -> do
                lv <- return $ fromJust $ M.lookup lv0 $ _ptcAmtVars cx_state
                rv <- return $ fromJust $ M.lookup rv0 $ _ptcFracVars cx_state

                S.put $ cx_state {
                  _ptcTypes = types',
                  _ptcFracVars = M.insert var (PSFracVar ix) $ _ptcFracVars cx_state,
                  _ptcTxnSoFar = txn {
                    _ptcFracOps = _ptcFracOps txn ++ [PSFracAmtTimesOp lv rv]
                  }
                }

              _ -> undefined

          _ -> undefined

        MinusExpr l r -> case tp of
          AmountType -> do
            let (ArithVar li) = l
            let (Just lv) = M.lookup li $ _ptcAmtVars cx_state
            let (ArithVar ri) = r
            let (Just rv) = M.lookup ri $ _ptcAmtVars cx_state

            let ix = _polNumAmtGlobals final_script + length (_ptcAmtOps txn)

            S.put $ cx_state {
              _ptcTypes = types',
              _ptcAmtVars = M.insert var (PSAmtVar ix) $ _ptcAmtVars cx_state,
              _ptcTxnSoFar = txn {
                _ptcAmtOps = _ptcAmtOps txn ++ [PSAmtMinusOp lv rv]
              }
            }
          _ -> undefined

        RoundExpr v -> do
          let (ArithVar fi) = v
          let (Just fv) = M.lookup fi $ _ptcFracVars cx_state
          let ix = _polNumAmtGlobals final_script + length (_ptcAmtOps txn)

          S.put $ cx_state {
            _ptcTypes = types',
            _ptcAmtVars = M.insert var (PSAmtVar ix) $ _ptcAmtVars cx_state,
            _ptcTxnSoFar = txn {
              _ptcAmtOps = _ptcAmtOps txn ++ [PSAmtRoundOp fv]
            }
          }

        AmountField res -> do
          let (Just res_var) = M.lookup res $ _ptcResVars cx_state
          ix <- return $ _polNumAmtGlobals final_script + length (_ptcAmtOps txn)

          S.put $ cx_state {
            _ptcTypes = types',
            _ptcAmtVars = M.insert var (PSAmtVar ix) $ _ptcAmtVars cx_state,
            _ptcTxnSoFar = txn {
              _ptcAmtOps = _ptcAmtOps txn ++ [PSAmtOfResOp res_var]
            }
          }

        OwnerField res -> do
          let (Just res_var) = M.lookup res $ _ptcResVars cx_state
          ix <- return $ _polNumIdGlobals final_script + length (_ptcIdOps txn)

          S.put $ (cx_state {
            _ptcTypes = types',
            _ptcIdVars = M.insert var (PSIdVar ix) $ _ptcIdVars cx_state,
            _ptcTxnSoFar = txn {
              _ptcIdOps = _ptcIdOps txn ++ [PSOwnerOfOp res_var]
            }
          })

        ArithVar v0 ->
          case (M.lookup v0 $ _ptcTypes cx_state) of
            Just FractionType -> do
              ix <- return $ _polNumFracGlobals final_script + length (_ptcFracOps txn)
              v <- return $ fromJust $ M.lookup v0 $ _ptcFracVars cx_state

              S.put $ cx_state {
                _ptcTypes = types',
                _ptcFracVars = M.insert var (PSFracVar ix) $ _ptcFracVars cx_state,
                _ptcTxnSoFar = txn {
                  _ptcFracOps = _ptcFracOps txn ++ [PSFracVarOp v]
                }
              }

            Just AmountType -> do
              ix <- return $ _polNumAmtGlobals final_script + length (_ptcAmtOps txn)
              v <- return $ fromJust $ M.lookup v0 $ _ptcAmtVars cx_state

              S.put $ cx_state {
                _ptcTypes = types',
                _ptcAmtVars = M.insert var (PSAmtVar ix) $ _ptcAmtVars cx_state,
                _ptcTxnSoFar = txn {
                  _ptcAmtOps = _ptcAmtOps txn ++ [PSAmtVarOp v]
                }
              }

            Just IdentityType -> do
              ix <- return $ _polNumIdGlobals final_script + length (_ptcIdOps txn)
              v <- return $ fromJust $ M.lookup v0 $ _ptcIdVars cx_state

              S.put $ cx_state {
                _ptcTypes = types',
                _ptcIdVars = M.insert var (PSIdVar ix) $ _ptcIdVars cx_state,
                _ptcTxnSoFar = txn {
                  _ptcIdOps = _ptcIdOps txn ++ [PSIdVarOp v]
                }
              }

            Just ResourceTypeType -> do
              ix <- return $ _polNumResTypeGlobals final_script + length (_ptcRtOps txn)
              v <- return $ fromJust $ M.lookup v0 $ _ptcResTypeVars cx_state

              S.put $ cx_state {
                _ptcTypes = types',
                _ptcResTypeVars = M.insert var (PSResTypeVar ix) $ _ptcResTypeVars cx_state,
                _ptcTxnSoFar = txn {
                  _ptcRtOps = _ptcRtOps txn ++ [PSResTypeVarOp v]
                }
              }
            _ -> undefined

          -- (Just res_var) <- return $ M.lookup res $ _ptcResVars cx_state
          -- ix <- return $ _polNumIdGlobals final_script + length (_ptcIdOps txn)

          -- S.put $ cx_state {
          --   _ptcTypes = types',
          --   _ptcIdVars = M.insert var (PSIdVar ix) $ -- cx_state,
          --   _ptcTxnSoFar = txn {
          --     _ptcIdOps = _ptcIdOps txn ++ [PSOwnerOfOp res_var]
          --   }
          -- }

    convertStmt (AssertStmt bexp) = do
      bvar <- {-trace (show ctx) $-} {-trace (show stmt) $-} convertBexpr bexp
      cx_state@(PolicyTxnConversionState { _ptcTxnSoFar = txn }) <- S.get

      S.put $ cx_state {
        _ptcTxnSoFar = txn {
          _ptcAssertions = _ptcAssertions txn ++ [bvar]
        }
      }

    convertBexpr TrueExpr = do
      cx_state@(PolicyTxnConversionState { _ptcTxnSoFar = txn }) <- S.get
      ret <- return $ PSBoolVar $ length $ _ptcBoolOps txn

      S.put $ cx_state {
        _ptcTxnSoFar = txn {
          _ptcBoolOps = _ptcBoolOps txn ++ [PSBoolConstOp True]
        }
      }
      return ret

    convertBexpr FalseExpr = do
      cx_state@(PolicyTxnConversionState { _ptcTxnSoFar = txn }) <- S.get
      ret <- return $ PSBoolVar $ length $ _ptcBoolOps txn

      S.put $ cx_state {
        _ptcTxnSoFar = txn {
          _ptcBoolOps = _ptcBoolOps txn ++ [PSBoolConstOp False]
        }
      }
      return ret

    convertBexpr (NotExpr be) = do
      bvar <- convertBexpr be
      cx_state@(PolicyTxnConversionState { _ptcTxnSoFar = txn }) <- S.get
      ret <- return $ PSBoolVar $ length $ _ptcBoolOps txn

      S.put $ cx_state {
        _ptcTxnSoFar = txn {
          _ptcBoolOps = _ptcBoolOps txn ++ [PSNot bvar]
        }
      }
      return ret

    convertBexpr (AndExpr l r) = do
      lvar <- convertBexpr l
      rvar <- convertBexpr r
      cx_state@(PolicyTxnConversionState { _ptcTxnSoFar = txn }) <- S.get
      ret <- return $ PSBoolVar $ length $ _ptcBoolOps txn

      S.put $ cx_state {
        _ptcTxnSoFar = txn {
          _ptcBoolOps = _ptcBoolOps txn ++ [PSAnd lvar rvar]
        }
      }
      return ret

    convertBexpr (OrExpr l r) = do
      lvar <- convertBexpr l
      rvar <- convertBexpr r
      cx_state@(PolicyTxnConversionState { _ptcTxnSoFar = txn }) <- S.get
      ret <- return $ PSBoolVar $ length $ _ptcBoolOps txn

      S.put $ cx_state {
        _ptcTxnSoFar = txn {
          _ptcBoolOps = _ptcBoolOps txn ++ [PSOr lvar rvar]
        }
      }
      return ret

    convertBexpr (GeExpr l r) = {-trace (show be) $-} do
      cx_state@(PolicyTxnConversionState { _ptcTxnSoFar = txn }) <- S.get
      let (ArithVar lv0) = {-trace (show l) $-} l
      let (ArithVar rv0) = {-trace (show r) $-} r
      let ret = PSBoolVar $ length $ _ptcBoolOps txn

      case (M.lookup lv0 $ _ptcTypes cx_state, M.lookup rv0 $ _ptcTypes cx_state) of
        (Just FractionType,Just FractionType) -> {-trace (show tps) $-} do
          lv <- return $ fromJust $ {-(\x -> trace (show (lv,x)) x) $-} M.lookup lv0 $ _ptcFracVars cx_state
          rv <- return $ fromJust $ {-(\x -> trace (show (rv,x)) x) $-} M.lookup rv0 $ _ptcFracVars cx_state

          S.put $ cx_state {
            _ptcTxnSoFar = txn {
              _ptcBoolOps = _ptcBoolOps txn ++ [PSFracGe lv rv]
            }
          }
        (Just AmountType,Just AmountType) -> {-trace (show tps) $-} do
          lv <- return $ fromJust $ {-(\x -> trace (show (lv,x)) x) $-} M.lookup lv0 $ _ptcAmtVars cx_state
          rv <- return $ fromJust $ {-(\x -> trace (show (rv,x)) x) $-} M.lookup rv0 $ _ptcAmtVars cx_state

          S.put $ cx_state {
            _ptcTxnSoFar = txn {
              _ptcBoolOps = _ptcBoolOps txn ++ [PSAmtGe lv rv]
            }
          }
        (Just AmountType,Just FractionType) -> {-trace (show tps) $-} do
          lv <- return $ fromJust $ {-(\x -> trace (show (lv,x)) x) $-} M.lookup lv0 $ _ptcAmtVars cx_state
          rv <- return $ fromJust $ {-(\x -> trace (show (rv,x)) x) $-} M.lookup rv0 $ _ptcFracVars cx_state

          S.put $ cx_state {
            _ptcTxnSoFar = txn {
              _ptcBoolOps = _ptcBoolOps txn ++ [PSAmtFracGe lv rv]
            }
          }
        (Just FractionType,Just AmountType) -> {-trace (show tps) $-} do
          lv <- return $ fromJust $ {-(\x -> trace (show (lv,x)) x) $-} M.lookup lv0 $ _ptcFracVars cx_state
          rv <- return $ fromJust $ {-(\x -> trace (show (rv,x)) x) $-} M.lookup rv0 $ _ptcAmtVars cx_state

          S.put $ cx_state {
            _ptcTxnSoFar = txn {
              _ptcBoolOps = _ptcBoolOps txn ++ [PSFracAmtGe lv rv]
            }
          }
        _ -> {-trace (show be)-} undefined
      return ret

    convertBexpr (EqExpr l r) = do
      cx_state@(PolicyTxnConversionState { _ptcTxnSoFar = txn }) <- S.get
      let (ArithVar lv) = l
      let (ArithVar rv) = r
      let ret = PSBoolVar $ length $ _ptcBoolOps txn

      case (M.lookup lv $ _ptcTypes cx_state, M.lookup rv $ _ptcTypes cx_state) of
        (Just FractionType,Just FractionType) -> do
          lvar <- return $ fromJust $ M.lookup lv $ _ptcFracVars cx_state
          rvar <- return $ fromJust $ M.lookup rv $ _ptcFracVars cx_state

          S.put $ cx_state {
            _ptcTxnSoFar = txn {
              _ptcBoolOps = _ptcBoolOps txn ++ [PSFracEq lvar rvar]
            }
          }
        (Just AmountType,Just AmountType) -> do
          lvar <- return $ fromJust $ M.lookup lv $ _ptcAmtVars cx_state
          rvar <- return $ fromJust $ M.lookup rv $ _ptcAmtVars cx_state

          S.put $ cx_state {
            _ptcTxnSoFar = txn {
              _ptcBoolOps = _ptcBoolOps txn ++ [PSAmtEq lvar rvar]
            }
          }
        (Just ResourceTypeType,Just ResourceTypeType) -> do
          lvar <- return $ fromJust $ M.lookup lv $ _ptcResTypeVars cx_state
          rvar <- return $ fromJust $ M.lookup rv $ _ptcResTypeVars cx_state

          S.put $ cx_state {
            _ptcTxnSoFar = txn {
              _ptcBoolOps = _ptcBoolOps txn ++ [PSResTypeEq lvar rvar]
            }
          }
        (Just IdentityType,Just IdentityType) -> do
          lvar <- return $ fromJust $ M.lookup lv $ _ptcIdVars cx_state
          rvar <- return $ fromJust $ M.lookup rv $ _ptcIdVars cx_state

          S.put $ cx_state {
            _ptcTxnSoFar = txn {
              _ptcBoolOps = _ptcBoolOps txn ++ [PSIdEq lvar rvar]
            }
          }
        _ -> undefined
      return ret

    convertBexpr (GtExpr _ _) = undefined
    convertBexpr (LtExpr _ _) = undefined
    convertBexpr (LeExpr _ _) = undefined

convertScriptToPolf :: PolicyScript -> PolicyFile
convertScriptToPolf (PolicyScript { _polNumIdGlobals = num_id_globals
                 , _polNumResTypeGlobals = num_res_type_globals
                 , _polNumAmtGlobals = num_amt_globals
                 , _polNumFracGlobals = num_frac_globals
                 , _polInitCheck = init_txn
                 , _polTxns = txns})
  = final_polf
  where
    final_polf = PolicyFile { _polfBats = bats
                            , _polfTxns = map convert_txn (init_txn:txns)
                            , _polfGParams = gparams }

    idglobals   = map (\i -> T.pack $ "id"   ++ show i) [0..(num_id_globals      -1)]
    rtglobals   = map (\i -> T.pack $ "rt"   ++ show i) [0..(num_res_type_globals-1)]
    amtglobals  = map (\i -> T.pack $ "amt"  ++ show i) [0..(num_amt_globals     -1)]
    fracglobals = map (\i -> T.pack $ "frac" ++ show i) [0..(num_frac_globals    -1)]

    bats = BoundAssetTypeStmt { _batsType   = (rtglobals !! 0),
                                _batsIssuer = (idglobals !! 0) }
    gparams = map (\ (nm,tp) ->
      GlobalParamDecl { _gparamName = nm, _gparamType = tp,
                        _gparamInvs = [] }) $ (>>= id) $
      [ map (\x -> (x,IdentityType))     $ tail idglobals
      , map (\x -> (x,ResourceTypeType)) $ tail rtglobals
      , map (\x -> (x,AmountType))   amtglobals
      , map (\x -> (x,FractionType)) fracglobals
      ]

    convert_txn (PolicyTxnCheck
                { _ptcTxnName     = name
                , _ptcInParams    = inparams
                , _ptcOutParams   = outparams

                , _ptcIdOps       = idOps
                , _ptcRtOps       = rtOps
                , _ptcAmtOps      = amtOps
                , _ptcFracOps     = fracOps
                , _ptcBoolOps     = boolOps

                , _ptcAssertions  = asserts
                , _ptcSignatures  = sigs
                , _ptcTxnTemplate = txn_ops
                }) = decl
        where
          decl = TxnDecl { _txnName = name
                         , _txnParams = namedIns ++ namedOuts
                         , _txnRequires = [], _txnEnsures = []
                         , _txnBody = body }

          namedIns = map (\ (ix,PSResTypeVar tp) -> TxnParamDecl
                            { _txnparamIn = True, _txnparamOut = False
                            , _txnparamName = T.pack $ "in" ++ show ix
                            , _txnparamType = rtglobals !! tp })
                      $ zip [(0::Integer)..] inparams

          namedOuts = map (\ (ix,PSResTypeVar tp) -> TxnParamDecl
                            { _txnparamIn = False, _txnparamOut = True
                            , _txnparamName = T.pack $ "out" ++ show ix
                            , _txnparamType = rtglobals !! tp })
                      $ zip [(0::Integer)..] outparams

          body = (>>= id)
            [id_ops, rt_ops, amt_frac_ops, assert_ops
            , sig_ops, op_ops]

          bool_ops = go boolOps
            where
              go [] = []
              go ((PSBoolConstOp True) :ops') = TrueExpr : go ops'
              go ((PSBoolConstOp False):ops') = FalseExpr : go ops'
              go ((PSIdEq (PSIdVar lv) (PSIdVar rv)):ops')
                = (EqExpr (ArithVar $ id_vars !! lv)
                          (ArithVar $ id_vars !! rv)) : go ops'
              go ((PSAmtEq (PSAmtVar lv) (PSAmtVar rv)):ops')
                = (EqExpr (ArithVar $ amt_vars !! lv)
                          (ArithVar $ amt_vars !! rv)) : go ops'
              go ((PSFracEq (PSFracVar lv) (PSFracVar rv)):ops')
                = (EqExpr (ArithVar $ frac_vars !! lv)
                          (ArithVar $ frac_vars !! rv)) : go ops'
              go ((PSResTypeEq (PSResTypeVar lv) (PSResTypeVar rv)):ops')
                = (EqExpr (ArithVar $ rt_vars !! lv)
                          (ArithVar $ rt_vars !! rv)) : go ops'
              go ((PSAmtGe (PSAmtVar lv) (PSAmtVar rv)):ops')
                = (GeExpr (ArithVar $ amt_vars !! lv)
                          (ArithVar $ amt_vars !! rv)) : go ops'
              go ((PSFracGe (PSFracVar lv) (PSFracVar rv)):ops')
                = (GeExpr (ArithVar $ frac_vars !! lv)
                          (ArithVar $ frac_vars !! rv)) : go ops'
              go ((PSFracAmtGe (PSFracVar lv) (PSAmtVar rv)):ops')
                = (GeExpr (ArithVar $ frac_vars !! lv)
                          (ArithVar $ amt_vars  !! rv)) : go ops'
              go ((PSAmtFracGe (PSAmtVar lv) (PSFracVar rv)):ops')
                = (GeExpr (ArithVar $ amt_vars  !! lv)
                          (ArithVar $ frac_vars !! rv)) : go ops'
              go ((PSNot (PSBoolVar bv)):ops')
                = (NotExpr $ bool_ops !! bv) : go ops'
              go ((PSAnd (PSBoolVar lv) (PSBoolVar rv)):ops')
                = (AndExpr (bool_ops !! lv) (bool_ops !! rv)) : go ops'
              go ((PSOr (PSBoolVar lv) (PSBoolVar rv)):ops')
                = (OrExpr (bool_ops !! lv) (bool_ops !! rv)) : go ops'


          assert_ops = map go asserts
            where go (PSBoolVar bv) = AssertStmt $ bool_ops !! bv
          sig_ops = map go sigs
            where go (PSIdVar iv) = RequireSignatureStmt $ id_vars !! iv
          op_ops = map go txn_ops
            where
              go (PSIssue (PSAmtVar av) (PSResTypeVar rtv) (PSResVar rv))
                = IssueStmt (ArithVar $ amt_vars !! av)
                            (rt_vars  !! rtv)
                            (_txnparamName $ (namedIns ++ namedOuts) !! rv)
              go (PSTransfer (PSAmtVar av) (PSResVar src_rv) Nothing)
                = TransferStmt (ArithVar $ amt_vars !! av)
                               (_txnparamName $ (namedIns ++ namedOuts) !! src_rv)
                               Nothing
              go (PSTransfer (PSAmtVar av) (PSResVar src_rv)
                             (Just (PSResVar dst_rv)))
                = TransferStmt (ArithVar $ amt_vars !! av)
                               (_txnparamName $ (namedIns ++ namedOuts) !! src_rv)
                               (Just $ _txnparamName $ (namedIns ++ namedOuts) !! dst_rv)

          id_ops = map (\ (var,expr) -> LocalStmt var (Just IdentityType) expr) base_id_ops
          id_vars = idglobals ++ map fst base_id_ops
          base_id_ops = zip (map (\x -> T.pack $ "id_local" ++ show x) [(0::Integer)..]) $ map go idOps
            where go (PSOwnerOfOp (PSResVar rv)) = OwnerField $ _txnparamName $ (namedIns ++ namedOuts) !! rv
                  go (PSIdVarOp   (PSIdVar  iv)) = ArithVar   $ id_vars !! iv

          rt_ops = map (\ (var,expr) -> LocalStmt var (Just ResourceTypeType) expr) base_rt_ops
          rt_vars = rtglobals ++ map fst base_rt_ops
          base_rt_ops = zip (map (\x -> T.pack $ "rt_local" ++ show x) [(0::Integer)..]) $ map go rtOps
            where go (PSTypeOfOp     (PSResVar     rv))  = ArithVar $ _txnparamType $ (namedIns ++ namedOuts) !! rv
                  go (PSResTypeVarOp (PSResTypeVar rtv)) = ArithVar $ rt_vars !! rtv

          amt_frac_ops = map (\ (var,tp,expr) -> LocalStmt var tp expr) base_amt_frac_ops
          amt_vars = amtglobals ++ map fst base_amt_ops
          frac_vars = fracglobals ++ map fst base_frac_ops
          (base_amt_frac_ops,base_amt_ops,base_frac_ops) = (op_types ops, op_lefts ops, op_rights ops)
            where
              op_types [] = []
              op_types ((n, Left op):ops') = (n,Just AmountType,op) : op_types ops'
              op_types ((n, Right op):ops') = (n,Just FractionType,op) : op_types ops'

              op_lefts [] = []
              op_lefts ((n,Left op):ops') = (n,op):op_lefts ops'
              op_lefts (_:ops') = op_lefts ops'

              op_rights [] = []
              op_rights ((n,Right op):ops') = (n,op):op_rights ops'
              op_rights (_:ops') = op_rights ops'

              ops = zip (map (\x -> T.pack $ "val_local" ++ show x) [(0::Integer)..])
                  $ goAmt (length amtglobals) (length fracglobals) amtOps fracOps

              goAmt _ _ [] [] = []
              goAmt a f [] fs = goFrac a f [] fs
              goAmt a f ((PSAmtVarOp (PSAmtVar  av)):as) fs
                | av < a
                = (Left $ ArithVar $ amt_vars !! av) : goAmt (a+1) f as fs
                | otherwise = undefined
              goAmt a f (aOp@(PSAmtRoundOp (PSFracVar fv)):as) fs
                | fv < f = (Left $ RoundExpr $ ArithVar $ frac_vars !! fv)
                         : goAmt (a+1) f as fs
                | otherwise = goFrac a f (aOp:as) fs
              goAmt a f ((PSAmtConstOp v):as) fs
                = (Left $ ConstAmountExpr $ toInteger v) : goAmt (a+1) f as fs
              goAmt a f ((PSAmtOfResOp (PSResVar rv)):as) fs
                    = (Left $ AmountField $ _txnparamName $ (namedIns ++ namedOuts) !! rv)
                    : goAmt (a+1) f as fs
              goAmt a f ((PSAmtPlusOp (PSAmtVar lv) (PSAmtVar rv)):as) fs
                | lv < a && rv < a
                = (Left $ PlusExpr (ArithVar $ amt_vars !! lv)
                                   (ArithVar $ amt_vars !! rv))
                  : goAmt (a+1) f as fs
                | otherwise = undefined
              goAmt a f ((PSAmtMinusOp (PSAmtVar lv) (PSAmtVar rv)):as) fs
                | lv < a && rv < a
                = (Left $ MinusExpr (ArithVar $ amt_vars !! lv)
                                    (ArithVar $ amt_vars !! rv))
                  : goAmt (a+1) f as fs
                | otherwise = undefined
              goAmt a f ((PSAmtTimesOp (PSAmtVar lv) (PSAmtVar rv)):as) fs
                | lv < a && rv < a
                = (Left $ TimesExpr (ArithVar $ amt_vars !! lv)
                                    (ArithVar $ amt_vars !! rv))
                  : goAmt (a+1) f as fs
                | otherwise = undefined

              goFrac _ _ [] [] = []
              goFrac a f as [] = goAmt a f as []
              goFrac a f as ((PSFracVarOp (PSFracVar  fv)):fs)
                = (Right $ ArithVar $ frac_vars !! fv) : goFrac a (f+1) as fs
              goFrac a f as ((PSFracConstOp v):fs)
                = (Right $ ConstFractionExpr v) : goFrac a (f+1) as fs
              goFrac a f as ((PSFracPlusOp (PSFracVar lv) (PSFracVar rv)):fs)
                | lv < f && rv < f
                = (Right $ PlusExpr  (ArithVar $ frac_vars !! lv)
                                     (ArithVar $ frac_vars !! rv))
                  : goFrac a (f+1) as fs
                | otherwise = undefined
              goFrac a f as ((PSFracTimesOp (PSFracVar lv) (PSFracVar rv)):fs)
                | lv < f && rv < f
                = (Right $ TimesExpr (ArithVar $ frac_vars !! lv)
                                     (ArithVar $ frac_vars !! rv))
                  : goFrac a (f+1) as fs
                | otherwise = undefined
              goFrac a f as (fOp@(PSFracTimesAmtOp (PSFracVar lv) (PSAmtVar rv)):fs)
                | lv < f && rv < a
                = (Right $ TimesExpr (ArithVar $ frac_vars !! lv)
                                     (ArithVar $ amt_vars  !! rv))
                  : goFrac a (f+1) as fs
                | lv < f = goAmt a f as (fOp:fs)
                | otherwise = undefined
              goFrac a f as (fOp@(PSFracAmtTimesOp (PSAmtVar lv) (PSFracVar rv)):fs)
                | lv < a && rv < f
                = (Right $ TimesExpr (ArithVar $ amt_vars  !! lv)
                                     (ArithVar $ frac_vars !! rv))
                  : goFrac a (f+1) as fs
                | rv < f = goAmt a f as (fOp:fs)
                | otherwise = undefined

pprintPolicyScript :: PolicyScript -> PP.Doc a
pprintPolicyScript ps =
  (PP.text "Policy" PP.<+>) $ ppBraces $ PP.nest 4 $ PP.vcat $ map (PP.<+> PP.comma)
    [ PP.text "num_id_globals:" PP.<+> PP.int (_polNumIdGlobals ps)
    , PP.text "num_rt_globals:" PP.<+> PP.int (_polNumResTypeGlobals ps)
    , PP.text "num_amt_globals:" PP.<+> PP.int (_polNumAmtGlobals ps)
    , PP.text "num_frac_globals:" PP.<+> PP.int (_polNumFracGlobals ps)
    , PP.text "init_check:" PP.<+> pprintPolicyTxnCheck (_polInitCheck ps)
    , (PP.text "txn_choices:" PP.<+> PP.text "vec!" PP.$$)
      $ PP.nest 1 $ ppBrackets $ PP.nest 4 $ PP.vcat $ map (PP.<+> PP.comma)
      $ map pprintPolicyTxnCheck (_polTxns ps)
    ]

ppBrackets :: PP.Doc a -> PP.Doc a
ppBrackets = (\x -> PP.lbrack PP.$+$ x PP.$+$ PP.rbrack)
ppBraces :: PP.Doc a -> PP.Doc a
ppBraces = (\x -> PP.lbrace PP.$+$ x PP.$+$ PP.rbrace)

pprintVec :: (a1 -> PP.Doc a2) -> [a1] -> PP.Doc a2
pprintVec pprintItem items = (PP.text "vec!" PP.<+>)
      $ ppBrackets $ PP.nest 4 $ PP.vcat $ map (PP.<+> PP.comma)
      $ map pprintItem items

pprintPolicyTxnCheck :: PolicyTxnCheck -> PP.Doc a
pprintPolicyTxnCheck ptc =
  (PP.text "TxnCheck" PP.<+>) $ ppBraces $ PP.nest 4 $ PP.vcat $ map (PP.<+> PP.comma)
    [ PP.text "name:" PP.<+> PP.doubleQuotes (PP.text $ T.unpack $ _ptcTxnName ptc) PP.<> PP.text ".to_string()"
    , (PP.text "in_params:" PP.<+>) $ pprintVec pprintResTypeVar $ _ptcInParams ptc
    , (PP.text "out_params:" PP.<+>) $ pprintVec pprintResTypeVar $ _ptcOutParams ptc

    , (PP.text "id_ops:" PP.<+>) $ pprintVec pprintIdOp $ _ptcIdOps ptc
    , (PP.text "rt_ops:" PP.<+>) $ pprintVec pprintResTypeOp $ _ptcRtOps ptc
    , (PP.text "fraction_ops:" PP.<+>) $ pprintVec pprintFracOp $ _ptcFracOps ptc
    , (PP.text "amount_ops:" PP.<+>) $ pprintVec pprintAmtOp $ _ptcAmtOps ptc
    , (PP.text "bool_ops:" PP.<+>) $ pprintVec pprintBoolOp $ _ptcBoolOps ptc
    , (PP.text "assertions:" PP.<+>) $ pprintVec pprintBoolVar $ _ptcAssertions ptc
    , (PP.text "required_signatures:" PP.<+>) $ pprintVec pprintIdVar $ _ptcSignatures ptc

    , (PP.text "txn_template:" PP.<+>) $ pprintVec pprintTxnOp $ _ptcTxnTemplate ptc
    ]

pprintTxnOp :: PSTxnOp -> PP.Doc a
pprintTxnOp (PSIssue amt restype res)
  = (PP.text "TxnOp::Issue" PP.<>) $ PP.parens $ PP.hsep
    $ map (PP.<> PP.comma) [pprintAmtVar amt, pprintResTypeVar restype, pprintResVar res]
pprintTxnOp (PSTransfer amt src dst)
  = (PP.text "TxnOp::Transfer" PP.<>) $ PP.parens $ PP.hsep
    $ map (PP.<> PP.comma) [pprintAmtVar amt, pprintResVar src,
        maybe (PP.text "None")
              ((PP.text "Some" PP.<>) . PP.parens . pprintResVar)
              dst
        ]

pprintBoolOp :: PSBoolOp -> PP.Doc a
pprintBoolOp (PSBoolConstOp bc)
  = PP.text "BoolOp::Const" PP.<> PP.parens (PP.text $ if bc then "true" else "false")
pprintBoolOp (PSIdEq l r)
  = PP.text "BoolOp::IdEq" PP.<> PP.parens (pprintIdVar l PP.<> PP.comma PP.<+> pprintIdVar r)
pprintBoolOp (PSAmtEq l r)
  = PP.text "BoolOp::AmtEq" PP.<> PP.parens (pprintAmtVar l PP.<> PP.comma PP.<+> pprintAmtVar r)
pprintBoolOp (PSFracEq l r)
  = PP.text "BoolOp::FracEq" PP.<> PP.parens (pprintFracVar l PP.<> PP.comma PP.<+> pprintFracVar r)
pprintBoolOp (PSResTypeEq l r)
  = PP.text "BoolOp::ResourceTypeEq" PP.<> PP.parens (pprintResTypeVar l PP.<> PP.comma PP.<+> pprintResTypeVar r)
pprintBoolOp (PSNot bv)
  = PP.text "BoolOp::Not" PP.<> PP.parens (pprintBoolVar bv)
pprintBoolOp (PSAnd l r)
  = PP.text "BoolOp::And" PP.<> PP.parens (pprintBoolVar l PP.<> PP.comma PP.<+> pprintBoolVar r)
pprintBoolOp (PSOr l r)
  = PP.text "BoolOp::Or" PP.<> PP.parens (pprintBoolVar l PP.<> PP.comma PP.<+> pprintBoolVar r)
pprintBoolOp (PSAmtGe l r)
  = PP.text "BoolOp::AmtGe" PP.<> PP.parens (pprintAmtVar l PP.<> PP.comma PP.<+> pprintAmtVar r)
pprintBoolOp (PSFracGe l r)
  = PP.text "BoolOp::FracGe" PP.<> PP.parens (pprintFracVar l PP.<> PP.comma PP.<+> pprintFracVar r)
pprintBoolOp (PSFracAmtGe l r)
  = PP.text "BoolOp::FracAmtGe" PP.<> PP.parens (pprintFracVar l PP.<> PP.comma PP.<+> pprintAmtVar r)
pprintBoolOp (PSAmtFracGe l r)
  = PP.text "BoolOp::AmtFracGe" PP.<> PP.parens (pprintAmtVar l PP.<> PP.comma PP.<+> pprintFracVar r)

pprintResTypeOp :: PSResTypeOp -> PP.Doc a
pprintResTypeOp (PSTypeOfOp res)
  = PP.text "ResourceTypeOp::TypeOfResource" PP.<> PP.parens (pprintResVar res)
pprintResTypeOp (PSResTypeVarOp rt_var)
  = PP.text "ResourceTypeOp::Var" PP.<> PP.parens (pprintResTypeVar rt_var)

pprintIdOp :: PSIdOp -> PP.Doc a
pprintIdOp (PSIdVarOp idvar)
  = PP.text "IdOp::Var" PP.<> PP.parens (pprintIdVar idvar)
pprintIdOp (PSOwnerOfOp res)
  = PP.text "IdOp::OwnerOf" PP.<> PP.parens (pprintResVar res)

pprintFracOp :: PSFracOp -> PP.Doc a
pprintFracOp (PSFracVarOp fv)
  = PP.text "FractionOp::Var" PP.<> PP.parens (pprintFracVar fv)
pprintFracOp (PSFracConstOp fc)
  = (PP.text "FractionOp::Const" PP.<>)
    $ PP.parens $ (PP.text "Fraction::new" PP.<>) $ PP.parens
    $ PP.int (fromInteger $ numerator fc) PP.<> PP.comma PP.<+> PP.int (fromInteger $ denominator fc)
pprintFracOp (PSFracPlusOp l r)
  = (PP.text "FractionOp::Plus" PP.<>)
    $ PP.parens $ pprintFracVar l PP.<> PP.comma PP.<+> pprintFracVar r
pprintFracOp (PSFracTimesOp l r)
  = (PP.text "FractionOp::Times" PP.<>)
    $ PP.parens $ pprintFracVar l PP.<> PP.comma PP.<+> pprintFracVar r
pprintFracOp (PSFracTimesAmtOp l r)
  = (PP.text "FractionOp::TimesAmt" PP.<>)
    $ PP.parens $ pprintFracVar l PP.<> PP.comma PP.<+> pprintAmtVar r
pprintFracOp (PSFracAmtTimesOp l r)
  = (PP.text "FractionOp::AmtTimes" PP.<>)
    $ PP.parens $ pprintAmtVar l PP.<> PP.comma PP.<+> pprintFracVar r

pprintAmtOp :: PSAmtOp -> PP.Doc a
pprintAmtOp (PSAmtVarOp av)
  = PP.text "AmountOp::Var" PP.<> PP.parens (pprintAmtVar av)
pprintAmtOp (PSAmtConstOp n)
  = PP.text "AmountOp::Const" PP.<> PP.parens (PP.int n)
pprintAmtOp (PSAmtOfResOp res)
  = PP.text "AmountOp::AmountOf" PP.<> PP.parens (pprintResVar res)
pprintAmtOp (PSAmtPlusOp l r)
  = (PP.text "AmountOp::Plus" PP.<>) $ PP.parens
    $ pprintAmtVar l PP.<> PP.comma PP.<+> pprintAmtVar r
pprintAmtOp (PSAmtTimesOp l r)
  = (PP.text "AmountOp::Times" PP.<>) $ PP.parens
    $ pprintAmtVar l PP.<> PP.comma PP.<+> pprintAmtVar r
pprintAmtOp (PSAmtMinusOp l r)
  = (PP.text "AmountOp::Minus" PP.<>) $ PP.parens
    $ pprintAmtVar l PP.<> PP.comma PP.<+> pprintAmtVar r
pprintAmtOp (PSAmtRoundOp fv)
  = (PP.text "AmountOp::Round" PP.<>) $ PP.parens
    $ pprintFracVar fv

pprintBoolVar :: PSBoolVar -> PP.Doc a
pprintBoolVar (PSBoolVar n) = PP.text "BoolVar" PP.<> PP.parens (PP.int n)

pprintAmtVar :: PSAmtVar -> PP.Doc a
pprintAmtVar (PSAmtVar n) = PP.text "AmountVar" PP.<> PP.parens (PP.int n)

pprintFracVar :: PSFracVar -> PP.Doc a
pprintFracVar (PSFracVar n) = PP.text "FractionVar" PP.<> PP.parens (PP.int n)

pprintResTypeVar :: PSResTypeVar -> PP.Doc a
pprintResTypeVar (PSResTypeVar n) = PP.text "ResourceTypeVar" PP.<> PP.parens (PP.int n)

pprintResVar :: PSResVar -> PP.Doc a
pprintResVar (PSResVar n) = PP.text "ResourceVar" PP.<> PP.parens (PP.int n)

pprintIdVar :: PSIdVar -> PP.Doc a
pprintIdVar (PSIdVar n) = PP.text "IdVar" PP.<> PP.parens (PP.int n)

compile :: (String -> IO ()) -> String -> IO (Either () PolicyScript)
compile writeOut polf = do
  ast <- return $ P.parse parsePolicyFile "" polf
  case ast of
    Left err -> do
      writeOut $ "parse error: " ++ show err
      return $ Left ()
    Right ast' -> do
      writeOut $ show ast'
      writeOut "\n\nPretty-printed:\n\n===============\n\n"
      rendered <- return $ PP.render $ pprintPolicyFile ast'
      writeOut rendered

      case (P.parse parsePolicyFile "" rendered) of
        Left err -> writeOut $ "reparse error: " ++ show err
        Right ast'' -> do
          writeOut $ "ASTs " ++ (if ast' == ast'' then "" else "don't ") ++ "match"

      do_compile <- return $ \ast'' -> foldM (\ast''' (name,f) -> do
        writeOut $ "\n\n" ++ name ++ ":\n\n===============\n\n"
        ast'''' <- return $ f ast'''
        astRendered <- return $ PP.render $ pprintPolicyFile $ ast''''
        writeOut astRendered
        return ast''''
        ) ast'' [ ("Explicit global_param init check", explicitGParamInit)
              , ("Explicit requires/ensures",over (polfTxns.traverse) explicitReqEns)
              , ("Make ALL expressions explicit", (over (polfTxns.traverse.txnBody) $ map (fromJust . fixAllExprs)))
              , ("Move old(...) expressions",(over (polfTxns.traverse.txnBody) moveOldExprs))
              , ("Split inout resources", over (polfTxns.traverse) splitInouts)
              , ("Make amount calculations explicit", (over (polfTxns.traverse.txnBody) explicitAmounts))
              , ("Move old(...) expressions",(over (polfTxns.traverse.txnBody) moveOldExprs))
              , ("Make a var for each subexpression", over (polfTxns.traverse) explicitSubExprsTxnDecl)
              , ("Expression preconditions", over (polfTxns.traverse.txnBody) addExprPreconds)
              , ("Make a var for each subexpression", over (polfTxns.traverse) explicitSubExprsTxnDecl)
              , ("typechecking", typecheck)
              , ("Reformat into locals -> asserts -> ops", over (polfTxns.traverse.txnBody) sortTxnStmts)
              , ("Remove redundant locals/asserts", over (polfTxns.traverse.txnBody) deleteRedundantStmts)
              , ("Simplify comparisons",
                 over (polfTxns.traverse.txnBody.traverse.txnStmt_bexpr) simplifyCompares)
              ]


      final_ast <- do_compile ast'

      policy_script <- return $ convertPolfToScript $ final_ast
      -- NOTE: currently this doesn't work right bc of "init_txn" and
      -- some differences in deduplication behavior
      -- policy_script2 <- convertPolfToScript <$> do_compile final_ast

      writeOut "\n\nResult:\n\n===============\n\n"
      writeOut $ PP.render $ pprintPolicyScript $ policy_script

      -- writeOut "\n\nResult2:\n\n===============\n\n"
      -- writeOut $ PP.render $ pprintPolicyScript $ policy_script2

      -- True <- return $ policy_script == policy_script2

      writeOut "\n\nDecompile:\n\n===============\n\n"
      dec_polf <- return $ convertScriptToPolf $ policy_script
      writeOut $ PP.render $ pprintPolicyFile $ dec_polf

      writeOut "\n\nRecompile:\n\n===============\n\n"
      rec_polf <- return $ convertPolfToScript $ dec_polf
      writeOut $ PP.render $ pprintPolicyScript rec_polf

      writeOut "\n\nDecompile2:\n\n===============\n\n"
      dec2_polf <- return $ convertScriptToPolf $ rec_polf
      writeOut $ PP.render $ pprintPolicyFile dec2_polf

      writeOut "\n\nRecompile2:\n\n===============\n\n"
      rec2_polf <- return $ convertPolfToScript $ dec_polf
      writeOut $ PP.render $ pprintPolicyScript rec2_polf

      True <- return $ dec_polf == dec2_polf
      True <- return $ rec_polf == rec2_polf
      True <- return $ rec_polf == policy_script

      writeOut "\n\nFinal result:\n\n===============\n\n"
      writeOut $ PP.render $ pprintPolicyScript $ policy_script

      return $ Right $ convertPolfToScript $ final_ast

