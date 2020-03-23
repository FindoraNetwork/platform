{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-} -- TODO: check if I need this
module PolicyLang where
import qualified Data.Text as T
import           Data.List (nub,isPrefixOf,sort)
import qualified Data.Map.Lazy as M
import qualified Control.Monad.State.Lazy as S
import qualified Text.Parsec   as P
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language (javaStyle)
import           Data.Maybe (maybeToList,fromMaybe,fromJust)
import           Control.Applicative ((<$>),(<*>),(*>))
import           Control.Monad (join, filterM, foldM, forM_)
import           System.Directory (getCurrentDirectory, getDirectoryContents, doesFileExist)
import           System.IO (hGetContents,openFile,IOMode(..),hFlush,stdout,stdin)
import           System.FilePath.Posix (takeBaseName)
import qualified Text.PrettyPrint.Annotated as PP
import           Control.Monad.Identity (runIdentity)
import           Alloyish(alloyish_main)
import           Data.Ratio(numerator,denominator)
import           Debug.Trace(trace)

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

simplifyCompares (NotExpr be) = NotExpr $ simplifyCompares be
simplifyCompares (AndExpr l r) = AndExpr (simplifyCompares l) (simplifyCompares r)
simplifyCompares (OrExpr l r) = OrExpr (simplifyCompares l) (simplifyCompares r)
simplifyCompares (GtExpr l r) = AndExpr (NotExpr (EqExpr l r)) (GeExpr l r)
simplifyCompares (GeExpr l r) = GeExpr l r
simplifyCompares (LeExpr l r) = GeExpr r l
simplifyCompares (LtExpr l r) = simplifyCompares (GtExpr r l)
simplifyCompares x = x

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

stmtVars ((LocalStmt var _ ae)) = [(var,ae)]
stmtVars _ = []

txnStmt_bexpr f (AssertStmt be) = AssertStmt <$> f be
txnStmt_bexpr _ txnStmt = pure txnStmt

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

freeVars = foldl (++) [] . map stmtVars

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

-- TODO: Identity/AssetTypeType arithmetic??!
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
              Nothing -> fromJust $ M.lookup v outRecs
              Just val -> val
            OwnerField v  -> OwnerField  $ case (M.lookup v recs) of
              Nothing -> fromJust $ M.lookup v outRecs
              Just val -> val
            _ -> over traverseArithSubExpr (replaceResource) x

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


explicitSubExprs body = fst $ flip S.runState (0,(M.fromList $ freeVars body) :: M.Map T.Text ArithExpr) $ goStmt body
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
    goAEinner exp@(ConstAmountExpr i) = do
      (ix,vars,vstmts) <- S.get
      (ix',varname,newstmts) <- return $ newvar ix vars exp
      vars' <- return $ M.insert varname exp vars
      vstmts' <- return $ vstmts ++ newstmts
      S.put (ix',vars',vstmts')
      return $ ArithVar varname
    goAEinner exp@(ConstFractionExpr i) = do
      (ix,vars,vstmts) <- S.get
      (ix',varname,newstmts) <- return $ newvar ix vars exp
      vars' <- return $ M.insert varname exp vars
      vstmts' <- return $ vstmts ++ newstmts
      S.put (ix',vars',vstmts')
      return $ ArithVar varname
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

  , _ptcNumInParams  :: Int
  , _ptcNumOutParams :: Int

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
                            , _ptcNumInParams = length $ filter _txnparamIn params
                            , _ptcNumOutParams = length $ filter _txnparamOut params
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
        Just dst -> fmap Just $ M.lookup dst $ _ptcResVars cx_state
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

    convertStmt (LocalStmt var tp exp) = do
      (tp) <- return $ fromJust tp
      cx_state@(PolicyTxnConversionState { _ptcTxnSoFar = txn }) <- S.get
      let Nothing = M.lookup var $ _ptcTypes cx_state

      let types' = M.insert var tp $ _ptcTypes cx_state

      case exp of
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
            let (ArithVar lv) = l
            let (ArithVar rv) = r
            case (M.lookup lv $ _ptcTypes cx_state, M.lookup rv $ _ptcTypes cx_state) of
              (Just FractionType,Just FractionType) -> do
                lv <- return $ fromJust $ M.lookup lv $ _ptcFracVars cx_state
                rv <- return $ fromJust $ M.lookup rv $ _ptcFracVars cx_state

                S.put $ cx_state {
                  _ptcTypes = types',
                  _ptcFracVars = M.insert var (PSFracVar ix) $ _ptcFracVars cx_state,
                  _ptcTxnSoFar = txn {
                    _ptcFracOps = _ptcFracOps txn ++ [PSFracTimesOp lv rv]
                  }
                }

              (Just FractionType,Just AmountType) -> do
                lv <- return $ fromJust $ M.lookup lv $ _ptcFracVars cx_state
                rv <- return $ fromJust $ M.lookup rv $ _ptcAmtVars cx_state

                S.put $ cx_state {
                  _ptcTypes = types',
                  _ptcFracVars = M.insert var (PSFracVar ix) $ _ptcFracVars cx_state,
                  _ptcTxnSoFar = txn {
                    _ptcFracOps = _ptcFracOps txn ++ [PSFracTimesAmtOp lv rv]
                  }
                }

              (Just AmountType,Just FractionType) -> do
                lv <- return $ fromJust $ M.lookup lv $ _ptcAmtVars cx_state
                rv <- return $ fromJust $ M.lookup rv $ _ptcFracVars cx_state

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

        ArithVar v ->
          case (M.lookup v $ _ptcTypes cx_state) of
            Just FractionType -> do
              ix <- return $ _polNumFracGlobals final_script + length (_ptcFracOps txn)
              v <- return $ fromJust $ M.lookup v $ _ptcFracVars cx_state

              S.put $ cx_state {
                _ptcTypes = types',
                _ptcFracVars = M.insert var (PSFracVar ix) $ _ptcFracVars cx_state,
                _ptcTxnSoFar = txn {
                  _ptcFracOps = _ptcFracOps txn ++ [PSFracVarOp v]
                }
              }

            Just AmountType -> do
              ix <- return $ _polNumAmtGlobals final_script + length (_ptcAmtOps txn)
              v <- return $ fromJust $ M.lookup v $ _ptcAmtVars cx_state

              S.put $ cx_state {
                _ptcTypes = types',
                _ptcAmtVars = M.insert var (PSAmtVar ix) $ _ptcAmtVars cx_state,
                _ptcTxnSoFar = txn {
                  _ptcAmtOps = _ptcAmtOps txn ++ [PSAmtVarOp v]
                }
              }

            Just IdentityType -> do
              ix <- return $ _polNumIdGlobals final_script + length (_ptcIdOps txn)
              v <- return $ fromJust $ M.lookup v $ _ptcIdVars cx_state

              S.put $ cx_state {
                _ptcTypes = types',
                _ptcIdVars = M.insert var (PSIdVar ix) $ _ptcIdVars cx_state,
                _ptcTxnSoFar = txn {
                  _ptcIdOps = _ptcIdOps txn ++ [PSIdVarOp v]
                }
              }

            Just ResourceTypeType -> do
              ix <- return $ _polNumResTypeGlobals final_script + length (_ptcRtOps txn)
              v <- return $ fromJust $ M.lookup v $ _ptcResTypeVars cx_state

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

    convertStmt stmt@(AssertStmt bexp) = do
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

    convertBexpr be@(GeExpr l r) = {-trace (show be) $-} do
      cx_state@(PolicyTxnConversionState { _ptcTxnSoFar = txn }) <- S.get
      let (ArithVar lv) = {-trace (show l) $-} l
      let (ArithVar rv) = {-trace (show r) $-} r
      let ret = PSBoolVar $ length $ _ptcBoolOps txn

      case (M.lookup lv $ _ptcTypes cx_state, M.lookup rv $ _ptcTypes cx_state) of
        tps@(Just FractionType,Just FractionType) -> {-trace (show tps) $-} do
          lv <- return $ fromJust $ {-(\x -> trace (show (lv,x)) x) $-} M.lookup lv $ _ptcFracVars cx_state
          rv <- return $ fromJust $ {-(\x -> trace (show (rv,x)) x) $-} M.lookup rv $ _ptcFracVars cx_state

          S.put $ cx_state {
            _ptcTxnSoFar = txn {
              _ptcBoolOps = _ptcBoolOps txn ++ [PSFracGe lv rv]
            }
          }
        tps@(Just AmountType,Just AmountType) -> {-trace (show tps) $-} do
          lv <- return $ fromJust $ {-(\x -> trace (show (lv,x)) x) $-} M.lookup lv $ _ptcAmtVars cx_state
          rv <- return $ fromJust $ {-(\x -> trace (show (rv,x)) x) $-} M.lookup rv $ _ptcAmtVars cx_state

          S.put $ cx_state {
            _ptcTxnSoFar = txn {
              _ptcBoolOps = _ptcBoolOps txn ++ [PSAmtGe lv rv]
            }
          }
        tps@(Just AmountType,Just FractionType) -> {-trace (show tps) $-} do
          lv <- return $ fromJust $ {-(\x -> trace (show (lv,x)) x) $-} M.lookup lv $ _ptcAmtVars cx_state
          rv <- return $ fromJust $ {-(\x -> trace (show (rv,x)) x) $-} M.lookup rv $ _ptcFracVars cx_state

          S.put $ cx_state {
            _ptcTxnSoFar = txn {
              _ptcBoolOps = _ptcBoolOps txn ++ [PSAmtFracGe lv rv]
            }
          }
        tps@(Just FractionType,Just AmountType) -> {-trace (show tps) $-} do
          lv <- return $ fromJust $ {-(\x -> trace (show (lv,x)) x) $-} M.lookup lv $ _ptcFracVars cx_state
          rv <- return $ fromJust $ {-(\x -> trace (show (rv,x)) x) $-} M.lookup rv $ _ptcAmtVars cx_state

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

ppBrackets = (\x -> PP.lbrack PP.$+$ x PP.$+$ PP.rbrack)
ppBraces = (\x -> PP.lbrace PP.$+$ x PP.$+$ PP.rbrace)

pprintPolicyTxnCheck ptc =
  (PP.text "TxnCheck" PP.<+>) $ ppBraces $ PP.nest 4 $ PP.vcat $ map (PP.<+> PP.comma)
    [ PP.text "name:" PP.<+> PP.doubleQuotes (PP.text $ T.unpack $ _ptcTxnName ptc) PP.<> PP.text ".to_string()"
    , PP.text "num_in_params:" PP.<+> PP.int (_ptcNumInParams ptc)
    , PP.text "num_out_params:" PP.<+> PP.int (_ptcNumOutParams ptc)

    , (PP.text "id_ops:" PP.<+> PP.text "vec!" PP.<+>)
      $ ppBrackets $ PP.nest 4 $ PP.vcat $ map (PP.<+> PP.comma)
      $ map pprintIdOp $ _ptcIdOps ptc
    , (PP.text "rt_ops:" PP.<+> PP.text "vec!" PP.<+>)
      $ ppBrackets $ PP.nest 4 $ PP.vcat $ map (PP.<+> PP.comma)
      $ map pprintResTypeOp $ _ptcRtOps ptc
    , (PP.text "fraction_ops:" PP.<+> PP.text "vec!" PP.<+>)
      $ ppBrackets $ PP.nest 4 $ PP.vcat $ map (PP.<+> PP.comma)
      $ map pprintFracOp $ _ptcFracOps ptc
    , (PP.text "amount_ops:" PP.<+> PP.text "vec!" PP.<+>)
      $ ppBrackets $ PP.nest 4 $ PP.vcat $ map (PP.<+> PP.comma)
      $ map pprintAmtOp $ _ptcAmtOps ptc
    , (PP.text "bool_ops:" PP.<+> PP.text "vec!" PP.<+>)
      $ ppBrackets $ PP.nest 4 $ PP.vcat $ map (PP.<+> PP.comma)
      $ map pprintBoolOp $ _ptcBoolOps ptc
    , (PP.text "assertions:" PP.<+> PP.text "vec!" PP.<+>)
      $ ppBrackets $ PP.nest 4 $ PP.vcat $ map (PP.<+> PP.comma)
      $ map pprintBoolVar $ _ptcAssertions ptc
    , (PP.text "required_signatures:" PP.<+> PP.text "vec!" PP.<+>)
      $ ppBrackets $ PP.nest 4 $ PP.vcat $ map (PP.<+> PP.comma)
      $ map pprintIdVar $ _ptcSignatures ptc

    , (PP.text "txn_template:" PP.<+> PP.text "vec!" PP.<+>)
      $ ppBrackets $ PP.nest 4 $ PP.vcat $ map (PP.<+> PP.comma)
      $ map pprintTxnOp $ _ptcTxnTemplate ptc
    ]

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

pprintResTypeOp (PSTypeOfOp res)
  = PP.text "ResourceTypeOp::TypeOfResource" PP.<> PP.parens (pprintResVar res)
pprintResTypeOp (PSResTypeVarOp rt_var)
  = PP.text "ResourceTypeOp::Var" PP.<> PP.parens (pprintResTypeVar rt_var)

pprintIdOp (PSIdVarOp idvar)
  = PP.text "IdOp::Var" PP.<> PP.parens (pprintIdVar idvar)
pprintIdOp (PSOwnerOfOp res)
  = PP.text "IdOp::OwnerOf" PP.<> PP.parens (pprintResVar res)

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

pprintBoolVar (PSBoolVar n) = PP.text "BoolVar" PP.<> PP.parens (PP.int n)
pprintAmtVar (PSAmtVar n) = PP.text "AmountVar" PP.<> PP.parens (PP.int n)
pprintFracVar (PSFracVar n) = PP.text "FractionVar" PP.<> PP.parens (PP.int n)
pprintResTypeVar (PSResTypeVar n) = PP.text "ResourceTypeVar" PP.<> PP.parens (PP.int n)
pprintResVar (PSResVar n) = PP.text "ResourceVar" PP.<> PP.parens (PP.int n)
pprintIdVar (PSIdVar n) = PP.text "IdVar" PP.<> PP.parens (PP.int n)

compile writeOut polf = do
  ast <- return $ P.parse parsePolicyFile "" polf
  case ast of
    Left err -> do
      writeOut $ "parse error: " ++ show err
      return $ Left ()
    Right ast -> do
      writeOut $ show ast
      writeOut "\n\nPretty-printed:\n\n===============\n\n"
      rendered <- return $ PP.render $ pprintPolicyFile ast
      writeOut rendered

      case (P.parse parsePolicyFile "" rendered) of
        Left err -> writeOut $ "reparse error: " ++ show err
        Right ast' -> do
          writeOut $ "ASTs " ++ (if ast == ast' then "" else "don't ") ++ "match"

      final_ast <- foldM (\ast (name,f) -> do
        writeOut $ "\n\n" ++ name ++ ":\n\n===============\n\n"
        ast <- return $ f ast
        astRendered <- return $ PP.render $ pprintPolicyFile $ ast
        writeOut astRendered
        return ast
        ) ast [ ("Explicit global_param init check", explicitGParamInit)
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

      writeOut "\n\nFinal result:\n\n===============\n\n"
      writeOut $ PP.render $ pprintPolicyScript $ convertPolfToScript $ final_ast

      return $ Right $ convertPolfToScript $ final_ast

