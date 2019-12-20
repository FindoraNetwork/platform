{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
module Main where
import qualified Data.Text as T
import           Data.List (nub,isPrefixOf,sort)
import qualified Data.Map.Lazy as M
import qualified Control.Monad.State.Lazy as S
import qualified Text.Parsec   as P
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language (javaStyle)
import           Data.Maybe (maybeToList,fromMaybe,fromJust)
import           Control.Applicative ((<$>),(<*>),(*>))
import           Control.Monad (join, filterM, foldM)
import           System.Directory (getCurrentDirectory, getDirectoryContents, doesFileExist)
import           System.IO (hGetContents,openFile,IOMode(..),hFlush,stdout,stdin)
import           System.FilePath.Posix (takeBaseName)
import qualified Text.PrettyPrint.Annotated as PP
import           Control.Monad.Identity (runIdentity)
import           Alloyish(alloyish_main)

policy_lang = P.makeTokenParser $ javaStyle
  { P.reservedNames = [ "param", "global_param", "local", "resource",
                        "credential", "in", "out", "inout" ,
                        "init_txn",
                        "bound_asset_type", "issued_by", "asset",
                        "txn", "assert", "ensures", "require",
                        "require_signature",
                        "of", "old", "new", "round",
                        "transfer", "issue",
                        "Fraction", "Amount", "Identity", "ResourceType",
                        "ALL", "BURN_ADDRESS", "owner", "amount",
                        "true", "false"
                      ]
  , P.reservedOpNames = [ "->", "*", "+", "-", "==", ">=", "<=", "<",
                          ">", ":", "&&", "||"
                        ]
  , P.caseSensitive = True
  }

polReserved = P.reserved policy_lang
polId = P.identifier policy_lang
polOp = P.reservedOp policy_lang
polBop op parseExpr = do
  e1 <- parseExpr
  polOp op
  e2 <- parseExpr
  return (e1,e2)

data BoundAssetTypeStmt = BoundAssetTypeStmt { _batsType :: T.Text, _batsIssuer :: T.Text }
  deriving (Eq,Show,Read)

parseBats = do
  polReserved "bound_asset_type"
  tp <- polId
  P.braces policy_lang (return ())
  polReserved "issued_by"
  issuer <- polId
  P.semi policy_lang
  return $ BoundAssetTypeStmt (T.pack tp) (T.pack issuer)

pprintBats (BoundAssetTypeStmt tp iss) = PP.hsep
  [ PP.text "bound_asset_type"
  , PP.text (T.unpack tp)
  , PP.braces PP.empty
  , PP.text "issued_by"
  , PP.text (T.unpack iss)
  ]

data DataType = FractionType
              | ResourceTypeType
              | IdentityType
              | AmountType
  deriving (Eq,Show,Read)

parseDataType
  = foldl1 (\x y -> P.try x P.<|> y) $
  [ const FractionType <$> polReserved "Fraction"
  , const ResourceTypeType <$> polReserved "ResourceType"
  , const IdentityType <$> polReserved "Identity"
  , const AmountType <$> polReserved "Amount"
  ]

pprintDataType FractionType = PP.text "Fraction"
pprintDataType ResourceTypeType = PP.text "ResourceType"
pprintDataType IdentityType = PP.text "Identity"
pprintDataType AmountType = PP.text "Amount"

data ArithExpr = ConstAmountExpr   Integer
               | ConstFractionExpr Rational
               | PlusExpr  ArithExpr ArithExpr
               | MinusExpr ArithExpr ArithExpr
               | TimesExpr ArithExpr ArithExpr
               | RoundExpr ArithExpr
               | OldExpr ArithExpr
               | AllExpr
               | ArithVar  T.Text
               | AmountField T.Text
               | OwnerField T.Text -- TODO: This isn't arithmetic!
  deriving (Eq,Show,Read)

parseSimpleArithExpr
  = foldl1 (\x y -> P.try x P.<|> y) $
  [ ConstFractionExpr . toRational <$> P.float policy_lang-- TODO: don't go through float maybe?
  , ConstAmountExpr <$> P.integer policy_lang
  , AmountField . T.pack <$> (polId <* (polOp "." >> polReserved "amount"))
  , OwnerField  . T.pack <$> (polId <* (polOp "." >> polReserved "owner"))
  , ArithVar    . T.pack <$> polId
  , RoundExpr <$> (polReserved "round" >> P.parens policy_lang parseArithExpr)
  , OldExpr <$> (polReserved "old" >> P.parens policy_lang parseArithExpr)
  , const AllExpr <$> (polReserved "ALL")
  , P.parens policy_lang parseArithExpr
  ]

parseArithExpr
  = foldl1 (\x y -> P.try x P.<|> y) $
  [ uncurry PlusExpr <$> polBop "+" parseSimpleArithExpr
  , uncurry MinusExpr <$> polBop "-" parseSimpleArithExpr
  , uncurry TimesExpr <$> polBop "*" parseSimpleArithExpr
  , parseSimpleArithExpr
  ]

pprintArithExpr (ConstAmountExpr i) = PP.integer i
pprintArithExpr (ConstFractionExpr f) = PP.double $ fromRational f
pprintArithExpr (AllExpr) = PP.text "ALL"
pprintArithExpr (ArithVar v) = PP.text $ T.unpack v
pprintArithExpr (AmountField v) = (PP.text $ T.unpack v) PP.<> PP.text ".amount"
pprintArithExpr (OwnerField v) = (PP.text $ T.unpack v) PP.<> PP.text ".owner"
pprintArithExpr (OldExpr e) = PP.text "old" PP.<> PP.parens (pprintArithExpr e)
pprintArithExpr (RoundExpr e) = PP.text "round" PP.<> PP.parens (pprintArithExpr e)
pprintArithExpr (PlusExpr l r) = PP.parens (pprintArithExpr l) PP.<+> PP.text "+" PP.<+> PP.parens (pprintArithExpr r)
pprintArithExpr (TimesExpr l r) = PP.parens (pprintArithExpr l) PP.<+> PP.text "*" PP.<+> PP.parens (pprintArithExpr r)
pprintArithExpr (MinusExpr l r) = PP.parens (pprintArithExpr l) PP.<+> PP.text "-" PP.<+> PP.parens (pprintArithExpr r)

traverseArithExpr f (ConstAmountExpr i) = f (ConstAmountExpr i)
traverseArithExpr f (ConstFractionExpr fr) = f (ConstFractionExpr fr)
traverseArithExpr f (AllExpr) = f AllExpr
traverseArithExpr f (ArithVar v) = f $ ArithVar v
traverseArithExpr f (AmountField v) = f $ AmountField v
traverseArithExpr f (OwnerField v) = f $ OwnerField v
traverseArithExpr f (OldExpr e) = OldExpr <$> f e
traverseArithExpr f (RoundExpr e) = RoundExpr <$> f e
traverseArithExpr f (PlusExpr l r) = PlusExpr <$> f l <*> f r
traverseArithExpr f (TimesExpr l r) = TimesExpr <$> f l <*> f r
traverseArithExpr f (MinusExpr l r) = MinusExpr <$> f l <*> f r

-- clumsy
traverseNonOldArithExpr f (ConstAmountExpr i) = f (ConstAmountExpr i)
traverseNonOldArithExpr f (ConstFractionExpr fr) = f (ConstFractionExpr fr)
traverseNonOldArithExpr f (AllExpr) = f AllExpr
traverseNonOldArithExpr f (ArithVar v) = f $ ArithVar v
traverseNonOldArithExpr f (AmountField v) = f $ AmountField v
traverseNonOldArithExpr f (OwnerField v) = f $ OwnerField v
traverseNonOldArithExpr f (OldExpr e) = pure $ OldExpr e
traverseNonOldArithExpr f (RoundExpr e) = RoundExpr <$> f e
traverseNonOldArithExpr f (PlusExpr l r) = PlusExpr <$> f l <*> f r
traverseNonOldArithExpr f (TimesExpr l r) = TimesExpr <$> f l <*> f r
traverseNonOldArithExpr f (MinusExpr l r) = MinusExpr <$> f l <*> f r


traverseArithSubExpr _ (ConstAmountExpr i) = pure (ConstAmountExpr i)
traverseArithSubExpr _ (ConstFractionExpr f) = pure (ConstFractionExpr f)
traverseArithSubExpr _ (AllExpr) = pure AllExpr
traverseArithSubExpr _ (ArithVar v) = pure $ ArithVar v
traverseArithSubExpr f (AmountField v) = pure $ AmountField v
traverseArithSubExpr f (OwnerField v) = pure $ OwnerField v
traverseArithSubExpr f (OldExpr e) = OldExpr <$> f e
traverseArithSubExpr f (RoundExpr e) = RoundExpr <$> f e
traverseArithSubExpr f (PlusExpr l r) = PlusExpr <$> f l <*> f r
traverseArithSubExpr f (TimesExpr l r) = TimesExpr <$> f l <*> f r
traverseArithSubExpr f (MinusExpr l r) = MinusExpr <$> f l <*> f r


data ArithType = ArithAmount
               | ArithAll
               | ArithFraction
               | ArithOwner
  deriving (Eq,Show,Read)

-- arithExprArithVars :: ArithExpr -> [T.Text]
-- arithExprArithVars (ArithVar v) = [v]
-- arithExprArithVars (PlusExpr l r) = arithExprArithVars l ++ arithExprArithVars r
-- arithExprArithVars (MinusExpr l r) = arithExprArithVars l ++ arithExprArithVars r
-- arithExprArithVars (TimesExpr l r) = arithExprArithVars l ++ arithExprArithVars r
-- arithExprArithVars (RoundExpr e) = arithExprArithVars e
-- arithExprArithVars (OldExpr e) = arithExprArithVars e
-- arithExprArithVars _ = []

-- arithExprType :: ArithExpr -> Maybe ArithType
-- arithExprType (ConstAmountExpr _) = Just ArithAmount
-- arithExprType (ConstFractionExpr _) = Just ArithFraction
-- arithExprType (AllExpr) = Just ArithAll
-- arithExprType (AllExpr) = Just ArithAll

data BoolExpr = TrueExpr
              | FalseExpr
              | NotExpr (BoolExpr)
              | AndExpr (BoolExpr) (BoolExpr)
              | OrExpr  (BoolExpr) (BoolExpr)
              | EqExpr (ArithExpr) (ArithExpr)
              | GtExpr (ArithExpr) (ArithExpr)
              | GeExpr (ArithExpr) (ArithExpr)
              | LtExpr (ArithExpr) (ArithExpr)
              | LeExpr (ArithExpr) (ArithExpr)
  deriving (Eq,Show,Read)

beTraverseArithExprs f (TrueExpr) = pure TrueExpr
beTraverseArithExprs f (FalseExpr) = pure FalseExpr
beTraverseArithExprs f (NotExpr be) = NotExpr <$> (beTraverseArithExprs f be)
beTraverseArithExprs f (AndExpr bl br) = AndExpr <$> (beTraverseArithExprs f bl) <*> (beTraverseArithExprs f br)
beTraverseArithExprs f (OrExpr bl br) = OrExpr <$> (beTraverseArithExprs f bl) <*> (beTraverseArithExprs f br)
beTraverseArithExprs f (EqExpr bl br) = EqExpr <$> f bl <*> f br
beTraverseArithExprs f (GtExpr bl br) = GtExpr <$> f bl <*> f br
beTraverseArithExprs f (GeExpr bl br) = GeExpr <$> f bl <*> f br
beTraverseArithExprs f (LtExpr bl br) = LtExpr <$> f bl <*> f br
beTraverseArithExprs f (LeExpr bl br) = LeExpr <$> f bl <*> f br

parseSimpleBoolExpr
  = foldl1 (\x y -> P.try x P.<|> y) $
  [ const TrueExpr <$> polReserved "true"
  , const FalseExpr <$> polReserved "false"
  , NotExpr <$> (polOp "!" >> parseSimpleBoolExpr)
  , uncurry EqExpr <$> polBop "==" parseSimpleArithExpr
  , uncurry GtExpr <$> polBop ">"  parseSimpleArithExpr
  , uncurry GeExpr <$> polBop ">=" parseSimpleArithExpr
  , uncurry LtExpr <$> polBop "<"  parseSimpleArithExpr
  , uncurry LeExpr <$> polBop "<=" parseSimpleArithExpr
  ]

parseBoolExpr
  = foldl1 (\x y -> P.try x P.<|> y) $
  [ uncurry AndExpr <$> polBop "&&" parseSimpleBoolExpr
  , uncurry OrExpr <$> polBop "||" parseSimpleBoolExpr
  , parseSimpleBoolExpr
  ]

pprintBoolExpr (TrueExpr) = PP.text "true"
pprintBoolExpr (FalseExpr) = PP.text "false"
pprintBoolExpr (NotExpr e) = PP.text "!" PP.<> PP.parens (pprintBoolExpr e)
pprintBoolExpr (AndExpr l r) = PP.parens (pprintBoolExpr l)  PP.<+> PP.text "&&" PP.<+> PP.parens (pprintBoolExpr r)
pprintBoolExpr (OrExpr l r)  = PP.parens (pprintBoolExpr l)  PP.<+> PP.text "||" PP.<+> PP.parens (pprintBoolExpr r)
pprintBoolExpr (EqExpr l r)  = PP.parens (pprintArithExpr l) PP.<+> PP.text "==" PP.<+> PP.parens (pprintArithExpr r)
pprintBoolExpr (GtExpr l r)  = PP.parens (pprintArithExpr l) PP.<+> PP.text ">"  PP.<+> PP.parens (pprintArithExpr r)
pprintBoolExpr (GeExpr l r)  = PP.parens (pprintArithExpr l) PP.<+> PP.text ">=" PP.<+> PP.parens (pprintArithExpr r)
pprintBoolExpr (LtExpr l r)  = PP.parens (pprintArithExpr l) PP.<+> PP.text "<"  PP.<+> PP.parens (pprintArithExpr r)
pprintBoolExpr (LeExpr l r)  = PP.parens (pprintArithExpr l) PP.<+> PP.text "<=" PP.<+> PP.parens (pprintArithExpr r)

data GlobalParamDecl = GlobalParamDecl
  { _gparamName :: T.Text, _gparamType :: DataType
  , _gparamInvs :: [BoolExpr] }
  deriving (Eq,Show,Read)

parseGParamDecl = do
  polReserved "global_param"
  varname <- polId
  polOp ":"
  vartype <- parseDataType
  invs <- ((fromMaybe [] <$>) $ P.optionMaybe $ P.braces policy_lang
                              $ parseBoolExpr `P.endBy` P.semi policy_lang
          )
  return $ GlobalParamDecl (T.pack varname) vartype invs

pprintGParamDecl (GlobalParamDecl name tp invs) =
  ((PP.text "global_param" PP.<+> PP.text (T.unpack name) PP.<> PP.text ":"
                          PP.<+> pprintDataType tp)
   PP.$+$ (PP.nest 4 $ PP.braces $ PP.vcat
                     $ map (\x -> pprintBoolExpr x PP.<> PP.semi) invs
          )
  )

-- Currently resources only
data TxnParamDecl = TxnParamDecl
  { _txnparamIn :: Bool, _txnparamOut :: Bool
  , _txnparamName :: T.Text, _txnparamType :: T.Text }
  deriving (Eq,Show,Read)

parseTxnParamDecl = do
  (isIn,isOut) <- P.choice [P.try (polReserved "in"    >> return (True,False))
                           ,P.try (polReserved "out"   >> return (False,True))
                           ,(polReserved "inout" >> return (True,True))
                           ]
  polReserved "resource"
  paramname <- polId
  polOp ":"
  paramtype <- polId
  return $ TxnParamDecl isIn isOut (T.pack paramname) (T.pack paramtype)

pprintTxnParamDecl (TxnParamDecl isIn isOut name tp)
  = PP.hsep [ (if isIn then PP.text "in" else PP.empty)
              PP.<> (if isOut then PP.text "out" else PP.empty)
            , PP.text "resource"
            , (PP.text $ T.unpack name) PP.<> PP.colon
            , PP.text $ T.unpack tp
            ]

data TxnStmt = AssertStmt BoolExpr
             | RequireSignatureStmt T.Text
             -- TODO: Currently only "arithmetic" locals
             | LocalStmt T.Text (Maybe DataType) ArithExpr
             | IssueStmt ArithExpr T.Text T.Text -- amount of asset-type to resource
             -- amount from resource1 to resource2 (Nothing => burn address)
             | TransferStmt ArithExpr T.Text (Maybe T.Text)
  deriving (Eq,Show,Read)

parseTxnStmt
  = foldl1 (\x y -> P.try x P.<|> y) $
  [ AssertStmt <$> (polReserved "assert" >> parseBoolExpr)
  , RequireSignatureStmt . T.pack <$> (polReserved "require_signature" >> polId)
  , do
      polReserved "local"
      name <- polId
      tp <- P.optionMaybe $ polOp ":" *> parseDataType
      polOp "="
      expr <- parseArithExpr
      return $ LocalStmt (T.pack name) tp expr
  , do
      polReserved "issue"
      amt <- parseSimpleArithExpr
      polReserved "of"
      asset_type <- polId
      polOp "->"
      dst <- polId
      return $ IssueStmt amt (T.pack asset_type) (T.pack dst)
  , do
      polReserved "transfer"
      amt <- parseSimpleArithExpr
      polReserved "of"
      src <- polId
      polOp "->"
      dst <- P.choice [polReserved "BURN_ADDRESS" >> return Nothing, Just <$> polId]
      return $ TransferStmt amt (T.pack src) (T.pack <$> dst)
  ]

pprintTxnStmt (AssertStmt be) = PP.text "assert" PP.<+> pprintBoolExpr be
pprintTxnStmt (RequireSignatureStmt name) = PP.text "require_signature" PP.<+> PP.text (T.unpack name)
pprintTxnStmt (LocalStmt varname tp aexp) = PP.hsep
  [ PP.text "local"
  , PP.text (T.unpack varname)
  , fromMaybe PP.empty $ (\tp -> PP.text ":" PP.<+> pprintDataType tp) <$> tp
  , PP.text "="
  , pprintArithExpr aexp
  ]
pprintTxnStmt (IssueStmt amt tp dst) = PP.hsep
  [ PP.text "issue"
  , PP.parens $ pprintArithExpr amt
  , PP.text "of"
  , PP.text $ T.unpack tp
  , PP.text "->"
  , PP.text $ T.unpack dst
  ]
pprintTxnStmt (TransferStmt amt src dst) = PP.hsep
  [ PP.text "transfer"
  , PP.parens $ pprintArithExpr amt
  , PP.text "of"
  , PP.text $ T.unpack src
  , PP.text "->"
  , PP.text $ fromMaybe "BURN_ADDRESS" $ T.unpack <$> dst
  ]

data TxnDecl = TxnDecl
  { _txnName :: T.Text, _txnParams :: [TxnParamDecl], _txnRequires :: [BoolExpr]
  , _txnEnsures :: [BoolExpr], _txnBody :: [TxnStmt] }
  deriving (Eq,Show,Read)

txnBody f decl = (\body' -> decl { _txnBody = body' }) <$> f (_txnBody decl)

parseTxnDecl = do
  polReserved "txn"
  name <- polId
  params <- P.parens policy_lang $ parseTxnParamDecl `P.sepEndBy` P.comma policy_lang
  requires <- (polReserved "requires" >> parseBoolExpr) `P.endBy` P.semi policy_lang
  ensures  <- (polReserved "ensures"  >> parseBoolExpr) `P.endBy` P.semi policy_lang
  body <- P.braces policy_lang $ parseTxnStmt `P.endBy` P.semi policy_lang
  return $ TxnDecl (T.pack name) params requires ensures body

pprintTxnDecl (TxnDecl name params requires ensures body) = PP.vcat
  [ PP.hsep [PP.text "txn", PP.text (T.unpack name),
             PP.parens $ PP.hsep
                       $ map (\x -> pprintTxnParamDecl x PP.<> PP.comma)
                             params]
  , PP.nest 4 $ PP.vcat
              $ map (\x -> PP.text "requires" PP.<+> pprintBoolExpr x
                                              PP.<> PP.semi)
                    requires
  , PP.nest 4 $ PP.vcat
              $ map (\x -> PP.text "ensures" PP.<+> pprintBoolExpr x
                                             PP.<> PP.semi)
                    ensures
  , PP.lbrace
  , PP.nest 4 $ PP.vcat $ (\x -> [PP.text ""] ++ x ++ [PP.text ""])
              $ map (\x -> pprintTxnStmt x PP.<> PP.semi) body
  , PP.rbrace
  ]

data PolicyFile = PolicyFile
  { _polfBats :: BoundAssetTypeStmt -- _bad_ name
  , _polfGParams :: [GlobalParamDecl]
  , _polfTxns :: [TxnDecl]
  }
  deriving (Eq,Show,Read)

-- TODO: makeLenses''
polfTxns f ast = (\txns' -> ast { _polfTxns = txns' }) <$> (f $ _polfTxns ast)

parsePolicyFile = do
  bats    <- parseBats
  gparams <- parseGParamDecl `P.endBy` P.semi policy_lang
  txns    <- P.many1 parseTxnDecl
  P.eof
  return $ PolicyFile bats gparams txns

pprintPolicyFile (PolicyFile bats gparams txns) = PP.vcat (
  [ pprintBats bats PP.<> PP.semi
  , PP.text ""
  ] ++ (map (\x -> pprintGParamDecl x PP.<> PP.semi) gparams) ++
  [ PP.text ""
  ] ++ (map (\x -> PP.text "" PP.$+$ pprintTxnDecl x) txns) ++
  [ PP.text ""
  ])

killAllExprs AllExpr = Nothing
killAllExprs x = Just x

fixAllExprs (AssertStmt be) = AssertStmt <$> beTraverseArithExprs killAllExprs be
fixAllExprs (RequireSignatureStmt i) = return $ (RequireSignatureStmt i)
fixAllExprs (LocalStmt v tp ae) = LocalStmt v tp <$> (traverseArithExpr killAllExprs ae)
fixAllExprs (IssueStmt amt tp dst) = (\amt -> IssueStmt amt tp dst)
                                    <$> (traverseArithExpr killAllExprs amt)
fixAllExprs (TransferStmt amt src dst) = (\amt -> TransferStmt amt src dst) <$> do
  amt <- traverseArithSubExpr killAllExprs amt
  return $ case amt of
    AllExpr -> AmountField src
    _ -> amt

freshname ix vars = head $ do
    incr <- [0..]
    ix <- return $ ix+incr
    ix' <- return $ ix+1
    varname <- return $ T.pack $ "__fresh" ++ show ix
    if (M.member varname vars) then [] else return (varname,ix')


newvar ix vars exp = (ix',varname,[LocalStmt varname Nothing exp])
  where (varname,ix') = freshname ix vars

-- NOTE: assumes that vars-for-subexpressions transformation has been
-- done
addExprPreconds lines = do
  l <- lines
  case l of
    LocalStmt v _ ae -> case ae of
      ConstAmountExpr i -> [AssertStmt (GeExpr (ConstAmountExpr i)
                                               (ConstAmountExpr 0)), l]
      ConstFractionExpr fr -> [AssertStmt (GeExpr (ConstFractionExpr fr)
                                                  (ConstFractionExpr 0)), l]
      MinusExpr le re -> [AssertStmt (GeExpr le re), l]
      -- NOTE: assumes that all other expressions are non-negative if
      -- their inputs are non-negative
      _ -> [l]
    _ -> return l

unifyTypes x y | x == y = Just x
unifyTypes AmountType FractionType = Just FractionType
unifyTypes FractionType AmountType = Just FractionType
unifyTypes _ _ = Nothing

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
            unifyTypes tp ae_tp
            return (M.insert v tp tpMap,l)
          IssueStmt amt _ _ -> do
            amt_tp <- typeForExpr tpMap amt
            unifyTypes AmountType amt_tp
            return (tpMap,l)
          TransferStmt amt _ _ -> do
            amt_tp <- typeForExpr tpMap amt
            unifyTypes AmountType amt_tp
            return (tpMap,l)

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
    go recs outRecs (l:ls) = l' : go recs' outRecs ls
      where
        recs' = case l of
          TransferStmt _ _ (Just v) -> M.insert v (fromJust $ M.lookup v outRecs) recs
          _ -> recs
        l' = case l of
          AssertStmt be
            -> AssertStmt $ over beTraverseArithExprs  (replaceResource recs) be
          LocalStmt v tp ae
            -> LocalStmt v tp $ replaceResource recs ae
          IssueStmt ae tp dst
            -> IssueStmt (replaceResource recs ae) tp dst
          TransferStmt ae src dst
            -> TransferStmt (replaceResource recs ae) src dst
          RequireSignatureStmt _ -> l
        replaceResource recs = over traverseArithExpr $ \x ->
          case x of
            AmountField v -> AmountField $ fromJust $ M.lookup v recs
            OwnerField v  -> OwnerField  $ case (M.lookup v recs) of
              Nothing -> fromJust $ M.lookup v outRecs
              Just val -> val
            _ -> over traverseArithSubExpr (replaceResource recs) x

    inConsumed = flip map (snd <$> M.toList inRecs) $ \x ->
      AssertStmt $ EqExpr (AmountField x) (ConstAmountExpr 0)

replaceVars m = over traverseArithExpr $ \x -> case x of
  ArithVar v -> fromMaybe x $ M.lookup v m
  _ -> x

replaceBeVars m = over beTraverseArithExprs $ replaceVars m

linLookup _ [] = Nothing
linLookup x ((k,v):vs) | x == k = Just v
linLookup x (_:vs) = linLookup x vs

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
          updates <- return $ M.insert dst ae' updates
          S.put updates
          return $ IssueStmt ae' tp dst
        TransferStmt ae src dst -> do
          ae' <- goAE ae
          updates <- S.get
          oldSrcVal <- return $ fromMaybe (OldExpr (AmountField src))
                              $ M.lookup src updates
          updates <- return $ M.insert src (MinusExpr oldSrcVal ae') updates
          updates <- return $ case dst of
            Nothing -> updates
            Just dst -> M.insert dst ae' updates
          S.put updates
          return $ TransferStmt ae' src dst
        _ -> return l
      (l':) <$> goStmt ls

    goAE :: ArithExpr -> S.State (M.Map T.Text ArithExpr) ArithExpr
    goAE ae = flip traverseNonOldArithExpr ae $ \ae -> do
      case ae of
        AmountField v -> do
          updates <- S.get
          return $ fromMaybe ae $ M.lookup v updates
        _ -> return ae

sortTxnStmts body = locals ++ nonops ++ ops
  where
    (locals,nonops,ops) = go body
    go [] = ([],[],[])
    go (l:ls) = let (locals,nonops,ops) = go ls in
      case l of
        (LocalStmt _ _ _) -> (l:locals,nonops,ops)
        (AssertStmt _) -> (locals,l:nonops,ops)
        (RequireSignatureStmt _) -> (locals,l:nonops,ops)
        (IssueStmt _ _ _) -> (locals,nonops,l:ops)
        (TransferStmt _ _ _) -> (locals,nonops,l:ops)

moveOldExprs body = go body
  where
    freshvar :: ArithExpr -> S.State (Integer,M.Map T.Text ArithExpr,[TxnStmt]) (T.Text,[TxnStmt])
    freshvar exp = do
      (ix,vars,stmts) <- S.get
      (ix',varname,newstmts) <- return $ newvar ix vars exp
      vars' <- return $ M.insert varname exp vars
      S.put (ix',vars',stmts)
      return (varname,newstmts)

    go ls = oldDecls ++ ls'
      where
        (ls',(_,_,oldDecls)) = flip S.runState (0,M.fromList [],[]) $ goStmt ls

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


explicitSubExprs body = fst $ flip S.runState (0,M.fromList ([] :: [(T.Text,ArithExpr)])) $ goStmt body
  where
    freshvar :: ArithExpr -> S.State (Integer,M.Map T.Text ArithExpr) (T.Text,[TxnStmt])
    freshvar exp = do
      (ix,vars) <- S.get
      (ix',varname,newstmts) <- return $ newvar ix vars exp
      vars' <- return $ M.insert varname exp vars
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
          (ix,vars,vstmts) <- S.get
          (ix',varname,newstmts) <- return $ newvar ix vars ae
          vars' <- return $ M.insert varname ae vars
          vstmts' <- return $ vstmts ++ newstmts
          S.put (ix',vars',vstmts')
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
    goAEinner ae = flip traverseArithSubExpr ae $ \exp -> do
      exp' <- goAEinner exp
      (ix,vars,vstmts) <- S.get
      (ix',varname,newstmts) <- return $ newvar ix vars exp'
      vars' <- return $ M.insert varname exp' vars
      vstmts' <- return $ vstmts ++ newstmts
      S.put (ix',vars',vstmts')
      return $ ArithVar varname

-- explicitSubExprsTxnDecl (TxnDecl name params requires ensures body)
--   = TxnDecl name params requires ensures $ explicitSubExprs body

over l f = runIdentity . l (return . f)

explicitSubExprsTxnDecl = over txnBody explicitSubExprs

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

typecheck ast = over (polfTxns) (map $ typecheckTxnDecl globalTypes) ast
  where
    globalTypes = M.insert (_batsIssuer $ _polfBats ast) IdentityType
                $ M.insert (_batsType   $ _polfBats ast) ResourceTypeType
                $ M.fromList
                $ (\decl -> (_gparamName decl, _gparamType decl))
                  <$> _polfGParams ast

-- main = alloyish_main
main = do
  polf <- hGetContents stdin
  ast <- return $ P.parse parsePolicyFile "" polf
  case ast of
    Left err -> putStrLn $ "parse error: " ++ show err
    Right ast -> do
      putStrLn $ show ast
      putStrLn "\n\nPretty-printed:\n\n===============\n\n"
      rendered <- return $ PP.render $ pprintPolicyFile ast
      putStrLn rendered

      case (P.parse parsePolicyFile "" rendered) of
        Left err -> putStrLn $ "reparse error: " ++ show err
        Right ast' -> do
          putStrLn $ "ASTs " ++ (if ast == ast' then "" else "don't ") ++ "match"

      final_ast <- foldM (\ast (name,f) -> do
        putStrLn $ "\n\n" ++ name ++ ":\n\n===============\n\n"
        ast <- return $ f ast
        astRendered <- return $ PP.render $ pprintPolicyFile $ ast
        putStrLn astRendered
        return ast
        ) ast [ ("Explicit global_param init check", explicitGParamInit)
              , ("Explicit requires/ensures",over (polfTxns.traverse) explicitReqEns)
              , ("Make ALL expressions explicit", (over (polfTxns.traverse.txnBody) $ map (fromJust . fixAllExprs)))
              , ("Make amount calculations explicit", (over (polfTxns.traverse.txnBody) explicitAmounts))
              , ("Move old(...) expressions",(over (polfTxns.traverse.txnBody) moveOldExprs))
              , ("Split inout resources", over (polfTxns.traverse) splitInouts)
              , ("Make a var for each subexpression", over (polfTxns.traverse) explicitSubExprsTxnDecl)
              , ("Expression preconditions", over (polfTxns.traverse.txnBody) addExprPreconds)
              , ("typechecking", typecheck)
              , ("Reformat into locals -> asserts -> ops", over (polfTxns.traverse.txnBody) sortTxnStmts)
              , ("Remove redundant locals/asserts", over (polfTxns.traverse.txnBody) deleteRedundantStmts)
              ]

      return ()

