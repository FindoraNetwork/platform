module PolicyLang where

import qualified Data.Text as T
import qualified Text.Parsec   as P
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language (javaStyle)
import           Data.Maybe (fromMaybe)
import           Control.Applicative ((<$>),(<*>),(*>))
import qualified Text.PrettyPrint.Annotated as PP

policy_lang :: P.TokenParser m
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

polReserved :: String -> P.Parsec String m ()
polReserved = P.reserved policy_lang

polId :: P.Parsec String m String
polId = P.identifier policy_lang

polOp :: String -> P.Parsec String m ()
polOp = P.reservedOp policy_lang

polBop :: String -> P.Parsec String m a -> P.Parsec String m (a,a)
polBop op parseExpr = do
  e1 <- parseExpr
  polOp op
  e2 <- parseExpr
  return (e1,e2)

data BoundAssetTypeStmt = BoundAssetTypeStmt { _batsType :: T.Text, _batsIssuer :: T.Text }
  deriving (Eq,Show,Read)

parseBats :: P.Parsec String m BoundAssetTypeStmt
parseBats = do
  polReserved "bound_asset_type"
  tp <- polId
  P.braces policy_lang (return ())
  polReserved "issued_by"
  issuer <- polId
  _ <- P.semi policy_lang
  return $ BoundAssetTypeStmt (T.pack tp) (T.pack issuer)

pprintBats :: BoundAssetTypeStmt -> PP.Doc a
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

parseDataType :: P.Parsec String m DataType
parseDataType
  = foldl1 (\x y -> P.try x P.<|> y) $
  [ const FractionType <$> polReserved "Fraction"
  , const ResourceTypeType <$> polReserved "ResourceType"
  , const IdentityType <$> polReserved "Identity"
  , const AmountType <$> polReserved "Amount"
  ]

pprintDataType :: DataType -> PP.Doc a
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

parseSimpleArithExpr :: P.Parsec String m ArithExpr
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

parseArithExpr :: P.Parsec String m ArithExpr
parseArithExpr
  = foldl1 (\x y -> P.try x P.<|> y) $
  [ uncurry PlusExpr <$> polBop "+" parseSimpleArithExpr
  , uncurry MinusExpr <$> polBop "-" parseSimpleArithExpr
  , uncurry TimesExpr <$> polBop "*" parseSimpleArithExpr
  , parseSimpleArithExpr
  ]

pprintArithExpr :: ArithExpr -> PP.Doc a
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

traverseArithExpr :: Applicative f => (ArithExpr -> f ArithExpr) -> ArithExpr -> f ArithExpr
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
traverseNonOldArithExpr :: Applicative f => (ArithExpr -> f ArithExpr) -> ArithExpr -> f ArithExpr
traverseNonOldArithExpr f (ConstAmountExpr i) = f (ConstAmountExpr i)
traverseNonOldArithExpr f (ConstFractionExpr fr) = f (ConstFractionExpr fr)
traverseNonOldArithExpr f (AllExpr) = f AllExpr
traverseNonOldArithExpr f (ArithVar v) = f $ ArithVar v
traverseNonOldArithExpr f (AmountField v) = f $ AmountField v
traverseNonOldArithExpr f (OwnerField v) = f $ OwnerField v
traverseNonOldArithExpr _ (OldExpr e) = pure $ OldExpr e
traverseNonOldArithExpr f (RoundExpr e) = RoundExpr <$> f e
traverseNonOldArithExpr f (PlusExpr l r) = PlusExpr <$> f l <*> f r
traverseNonOldArithExpr f (TimesExpr l r) = TimesExpr <$> f l <*> f r
traverseNonOldArithExpr f (MinusExpr l r) = MinusExpr <$> f l <*> f r

traverseArithSubExpr :: Applicative f => (ArithExpr -> f ArithExpr) -> ArithExpr -> f ArithExpr
traverseArithSubExpr _ (ConstAmountExpr i) = pure (ConstAmountExpr i)
traverseArithSubExpr _ (ConstFractionExpr f) = pure (ConstFractionExpr f)
traverseArithSubExpr _ (AllExpr) = pure AllExpr
traverseArithSubExpr _ (ArithVar v) = pure $ ArithVar v
traverseArithSubExpr _ (AmountField v) = pure $ AmountField v
traverseArithSubExpr _ (OwnerField v) = pure $ OwnerField v
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

simplifyCompares :: BoolExpr -> BoolExpr
simplifyCompares (NotExpr be) = NotExpr $ simplifyCompares be
simplifyCompares (AndExpr l r) = AndExpr (simplifyCompares l) (simplifyCompares r)
simplifyCompares (OrExpr l r) = OrExpr (simplifyCompares l) (simplifyCompares r)
simplifyCompares (GtExpr l r) = AndExpr (NotExpr (EqExpr l r)) (GeExpr l r)
simplifyCompares (GeExpr l r) = GeExpr l r
simplifyCompares (LeExpr l r) = GeExpr r l
simplifyCompares (LtExpr l r) = simplifyCompares (GtExpr r l)
simplifyCompares x = x

beTraverseArithExprs :: Applicative f => (ArithExpr -> f ArithExpr) -> BoolExpr -> f BoolExpr
beTraverseArithExprs _ (TrueExpr) = pure TrueExpr
beTraverseArithExprs _ (FalseExpr) = pure FalseExpr
beTraverseArithExprs f (NotExpr be) = NotExpr <$> (beTraverseArithExprs f be)
beTraverseArithExprs f (AndExpr bl br) = AndExpr <$> (beTraverseArithExprs f bl) <*> (beTraverseArithExprs f br)
beTraverseArithExprs f (OrExpr bl br) = OrExpr <$> (beTraverseArithExprs f bl) <*> (beTraverseArithExprs f br)
beTraverseArithExprs f (EqExpr bl br) = EqExpr <$> f bl <*> f br
beTraverseArithExprs f (GtExpr bl br) = GtExpr <$> f bl <*> f br
beTraverseArithExprs f (GeExpr bl br) = GeExpr <$> f bl <*> f br
beTraverseArithExprs f (LtExpr bl br) = LtExpr <$> f bl <*> f br
beTraverseArithExprs f (LeExpr bl br) = LeExpr <$> f bl <*> f br

parseSimpleBoolExpr :: P.Parsec String m BoolExpr
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

parseBoolExpr :: P.Parsec String m BoolExpr
parseBoolExpr
  = foldl1 (\x y -> P.try x P.<|> y) $
  [ uncurry AndExpr <$> polBop "&&" parseSimpleBoolExpr
  , uncurry OrExpr <$> polBop "||" parseSimpleBoolExpr
  , parseSimpleBoolExpr
  ]

pprintBoolExpr :: BoolExpr -> PP.Doc a
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

parseGParamDecl :: P.Parsec String m GlobalParamDecl
parseGParamDecl = do
  polReserved "global_param"
  varname <- polId
  polOp ":"
  vartype <- parseDataType
  invs <- ((fromMaybe [] <$>) $ P.optionMaybe $ P.braces policy_lang
                              $ parseBoolExpr `P.endBy` P.semi policy_lang
          )
  return $ GlobalParamDecl (T.pack varname) vartype invs

pprintGParamDecl :: GlobalParamDecl -> PP.Doc a
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

parseTxnParamDecl :: P.Parsec String m TxnParamDecl
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

pprintTxnParamDecl :: TxnParamDecl -> PP.Doc a
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

stmtVars :: TxnStmt -> [(T.Text, ArithExpr)]
stmtVars ((LocalStmt var _ ae)) = [(var,ae)]
stmtVars _ = []

txnStmt_bexpr :: Applicative f => (BoolExpr -> f BoolExpr) -> TxnStmt -> f TxnStmt
txnStmt_bexpr f (AssertStmt be) = AssertStmt <$> f be
txnStmt_bexpr _ txnStmt = pure txnStmt

parseTxnStmt :: P.Parsec String m TxnStmt
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

pprintTxnStmt :: TxnStmt -> PP.Doc a
pprintTxnStmt (AssertStmt be) = PP.text "assert" PP.<+> pprintBoolExpr be
pprintTxnStmt (RequireSignatureStmt name) = PP.text "require_signature" PP.<+> PP.text (T.unpack name)
pprintTxnStmt (LocalStmt varname tp aexp) = PP.hsep
  [ PP.text "local"
  , PP.text (T.unpack varname)
  , fromMaybe PP.empty $ (\t -> PP.text ":" PP.<+> pprintDataType t) <$> tp
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

txnBody :: Functor f => ([TxnStmt] -> f [TxnStmt]) -> TxnDecl -> f TxnDecl
txnBody f decl = (\body' -> decl { _txnBody = body' }) <$> f (_txnBody decl)

freeVars :: [TxnStmt] -> [(T.Text, ArithExpr)]
freeVars = foldl (++) [] . map stmtVars

parseTxnDecl :: P.Parsec String m TxnDecl
parseTxnDecl = do
  polReserved "txn"
  name <- polId
  params <- P.parens policy_lang $ parseTxnParamDecl `P.sepEndBy` P.comma policy_lang
  requires <- (polReserved "requires" >> parseBoolExpr) `P.endBy` P.semi policy_lang
  ensures  <- (polReserved "ensures"  >> parseBoolExpr) `P.endBy` P.semi policy_lang
  body <- P.braces policy_lang $ parseTxnStmt `P.endBy` P.semi policy_lang
  return $ TxnDecl (T.pack name) params requires ensures body

pprintTxnDecl :: TxnDecl -> PP.Doc a
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
polfTxns :: Functor f => ([TxnDecl] -> f [TxnDecl]) -> PolicyFile -> f PolicyFile
polfTxns f ast = (\txns' -> ast { _polfTxns = txns' }) <$> (f $ _polfTxns ast)

parsePolicyFile :: P.Parsec String m PolicyFile
parsePolicyFile = do
  bats    <- parseBats
  gparams <- parseGParamDecl `P.endBy` P.semi policy_lang
  txns    <- P.many1 parseTxnDecl
  P.eof
  return $ PolicyFile bats gparams txns

pprintPolicyFile :: PolicyFile -> PP.Doc a
pprintPolicyFile (PolicyFile bats gparams txns) = PP.vcat (
  [ pprintBats bats PP.<> PP.semi
  , PP.text ""
  ] ++ (map (\x -> pprintGParamDecl x PP.<> PP.semi) gparams) ++
  [ PP.text ""
  ] ++ (map (\x -> PP.text "" PP.$+$ pprintTxnDecl x) txns) ++
  [ PP.text ""
  ])


