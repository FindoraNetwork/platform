{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
module Main where
import qualified Data.Text as T
import           Data.List (nub,isPrefixOf,sort)
import qualified Data.Map.Lazy as M
import qualified Text.Parsec   as P
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language (javaStyle)
import           Data.Maybe (maybeToList,fromMaybe)
import           Control.Applicative ((<$>),(<*>),(*>))
import           Control.Monad (join, filterM)
import           System.Directory (getCurrentDirectory, getDirectoryContents, doesFileExist)
import           System.IO (hGetContents,openFile,IOMode(..),hFlush,stdout,stdin)
import           System.FilePath.Posix (takeBaseName)
import qualified Text.PrettyPrint.Annotated as PP

alloyish = P.makeTokenParser $ javaStyle
  { P.reservedNames = [ "sig", "abstract", "extends"
                    , "in", "let", "fun", "pred", "fact", "check"
                    , "all", "one", "some", "no", "sum"
                    , "none", "univ", "ident"
                    , "filter", "map"
                    , "fresh", "setexp", "relexp"
                    ]
  , P.caseSensitive = True
  }

policy_lang = P.makeTokenParser $ javaStyle
  { P.reservedNames = [ "param", "global_param", "local", "resource",
                        "credential", "in", "out", "inout" ,
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
  [ ConstAmountExpr <$> P.decimal policy_lang
  , ConstFractionExpr . toRational <$> P.float policy_lang-- TODO: don't go through float maybe?
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
             | LocalStmt T.Text ArithExpr -- TODO: Currently only arithmetic locals
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
      polOp "="
      expr <- parseArithExpr
      return $ LocalStmt (T.pack name) expr
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
pprintTxnStmt (LocalStmt varname aexp) = PP.hsep
  [ PP.text "local"
  , PP.text (T.unpack varname)
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
              $ map (\x -> PP.text "requires" PP.<> pprintBoolExpr x
                                              PP.<> PP.semi)
                    requires
  , PP.nest 4 $ PP.vcat
              $ map (\x -> PP.text "ensures" PP.<> pprintBoolExpr x
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

parseOp op = do
  theOp <- P.operator alloyish
  if theOp == op then return () else fail ("Expected '" ++ op ++ "'")

setBop op = do
  l <- parseLeftSetExp
  parseOp op
  r <- parseSetExp
  return (l,r)

setLeftBop op = do
  l <- parseSimpleSetExp
  parseOp op
  r <- parseSimpleSetExp
  return (l,r)

parseSimpleSetExp
  = foldl1 (\x y -> P.try x P.<|> y) $
  [ P.parens alloyish parseSetExp
  , ClosureSet <$> (parseOp "*" *> parseSimpleSetExp)
  , OneClosureSet <$> (parseOp "^" *> parseSimpleSetExp)
  , TransposeSet <$> (parseOp "~" *> parseSimpleSetExp)
  , AtomSet <$> (P.identifier alloyish)
  , const NoneSet <$> (P.reserved alloyish "none")
  , const UnivSet <$> (P.reserved alloyish "univ")
  , const IdentSet <$> (P.reserved alloyish "ident")
  ]

parseLeftSetExp
  = foldl1 (\x y -> P.try x P.<|> y) $
  [ uncurry (\x y -> JoinSet x (ClosureSet y)) <$> setLeftBop ".*"
  , uncurry (\x y -> JoinSet x (OneClosureSet y)) <$> setLeftBop ".^"
  , parseSimpleSetExp
  ]

parseSetExp
  = foldl1 (\x y -> P.try x P.<|> y) $
  [ uncurry JoinSet <$> setBop "."
  , uncurry ProdSet <$> setBop "->"
  , uncurry IsectSet <$> setBop "&"
  , uncurry UnionSet <$> setBop "+"
  , uncurry DiffSet <$> setBop "-"
  , do
    -- ctor <- (const FilterSet <$> P.try (P.reserved alloyish "filter"))
    --         P.<|> (const MapSet <$> P.reserved alloyish "map")
    P.reserved alloyish "filter"
    var <- P.identifier alloyish
    parseOp ":"
    baseexp <- parseSimpleSetExp
    relexp <- P.braces alloyish parseRelExp
    relexp <- return $ flip fmap relexp $ \vname ->
      if vname == var then Nothing else Just vname
    return $ FilterSet baseexp relexp
  , do
    -- ctor <- (const FilterSet <$> P.try (P.reserved alloyish "filter"))
    --         P.<|> (const MapSet <$> P.reserved alloyish "map")
    P.reserved alloyish "map"
    var <- P.identifier alloyish
    parseOp ":"
    baseexp <- parseSimpleSetExp
    setexp <- P.braces alloyish parseSetExp
    setexp <- return $ flip fmap setexp $ \vname ->
      if vname == var then Nothing else Just vname
    return $ MapSet baseexp setexp
  , parseLeftSetExp
  ]

relBop op = do
  l <- parseSimpleRelExp
  parseOp op
  r <- parseRelExp
  return (l,r)

parseSimpleRelExp
  = foldl1 (\x y -> P.try x P.<|> y) $
  [ P.parens alloyish parseRelExp
  , NoRel <$> (P.reserved alloyish "no" *> parseSimpleSetExp)
  , SomeRel <$> (P.reserved alloyish "some" *> parseSimpleSetExp)
  , LoneRel <$> (P.reserved alloyish "lone" *> parseSimpleSetExp)
  , OneRel <$> (P.reserved alloyish "one" *> parseSimpleSetExp)
  , uncurry EqRel <$> setBop "=="
  , uncurry NeqRel <$> setBop "!="
  , uncurry InRel <$> do
      l <- parseSimpleSetExp
      P.reserved alloyish "in"
      r <- parseSetExp
      return (l,r)
  , NotRel <$> (P.reserved alloyish "not" *> parseSimpleRelExp)
  ]

parseQRel qstr = do
  P.reserved alloyish qstr
  var <- P.identifier alloyish
  parseOp ":"
  baseexp <- parseSimpleSetExp
  relexp <- P.braces alloyish parseRelExp
  relexp <- return $ flip fmap relexp $ \vname ->
    if vname == var then Nothing else Just vname
  return (baseexp,relexp)

parseRelExp
  = foldl1 (\x y -> P.try x P.<|> y) $
  [ uncurry AndRel <$> relBop "&&"
  , uncurry OrRel <$> relBop "||"
  , uncurry ImplyRel <$> relBop "=>"
  , uncurry IffRel <$> relBop "<=>"
  , uncurry QAllRel <$> parseQRel "all"
  , uncurry QSomeRel <$> parseQRel "some"
  , uncurry QOneRel <$> parseQRel "one"
  , uncurry QLoneRel <$> parseQRel "lone"
  , uncurry QNoRel <$> parseQRel "no"
  , parseSimpleRelExp
  ]

data SetExp v
  = AtomSet v
  | NoneSet -- empty 1-ary relation
  | UnivSet -- Universe of 1-ary relations
  | IdentSet -- binary identity relation
  | JoinSet       (SetExp v) (SetExp v)
  | ProdSet       (SetExp v) (SetExp v)
  | IsectSet      (SetExp v) (SetExp v)
  | UnionSet      (SetExp v) (SetExp v)
  | DiffSet       (SetExp v) (SetExp v)
  | ClosureSet    (SetExp v)
  | OneClosureSet (SetExp v)
  | TransposeSet  (SetExp v)
  | FilterSet     (SetExp v) (RelExp (Maybe v))
  | MapSet        (SetExp v) (SetExp (Maybe v))
  deriving (Eq,Show,Read,Functor)

data RelExp v
  = InRel    (SetExp v) (SetExp v)
  | EqRel    (SetExp v) (SetExp v)
  | NeqRel   (SetExp v) (SetExp v)
  | SomeRel  (SetExp v)
  | NoRel    (SetExp v)
  | LoneRel  (SetExp v)
  | OneRel   (SetExp v)
  | NotRel   (RelExp v)
  | AndRel   (RelExp v) (RelExp v)
  | OrRel    (RelExp v) (RelExp v)
  | ImplyRel (RelExp v) (RelExp v)
  | IffRel   (RelExp v) (RelExp v)
  | QAllRel  (SetExp v) (RelExp (Maybe v))
  | QSomeRel (SetExp v) (RelExp (Maybe v))
  | QOneRel  (SetExp v) (RelExp (Maybe v))
  | QLoneRel (SetExp v) (RelExp (Maybe v))
  | QNoRel   (SetExp v) (RelExp (Maybe v))
  deriving (Eq,Show,Read,Functor)

data Env v dat = Env
  { envVars     :: M.Map T.Text [[v]] -- relations
  , envUniverse :: [v]
  , envExtra    :: M.Map v dat
  } deriving (Eq,Show,Read,Functor)

data Entry = Entry
  { entName     :: T.Text
  , entBaseSets :: [T.Text]
  , entEdges    :: M.Map T.Text [T.Text]
  , entData     :: M.Map T.Text T.Text
  } deriving (Eq,Show,Read)

freshEnvVar :: Env v dat -> T.Text
freshEnvVar env = head $ filter (not . (`elem` M.keys (envVars env)))
                       $ scanl (<>) "_" (repeat "_")

applyEntry :: Entry -> Env T.Text (M.Map T.Text T.Text) -> Env T.Text (M.Map T.Text T.Text)
applyEntry (Entry name bases edges dat)
           (Env{envVars=vars,envUniverse=univ,envExtra=extra})
  = Env
    { envVars = foldl (\x f -> f x) (M.insert name [[name]] vars)
                edgeUpdates
    , envUniverse = (if name `elem` univ then [] else [name]) ++ univ
                  ++ (filter (not . (`elem` (name:univ))) $
                      nub $ join $ M.elems edges)
    , envExtra = M.insert name dat extra
    }
  where
    edgeUpdates = foldl1 (++) $
      [ do
        (k,v) <- M.assocs edges
        other <- v
        return $ flip M.alter k $ Just . \case
          Nothing -> [[name,other]]
          Just es -> if [name,other] `elem` es
            then es else [name,other]:es
      , do
        b <- bases
        return $ flip M.alter b $ Just . \case
          Nothing -> [[name]]
          Just es -> if [name] `elem` es
            then es else [name]:es
      ]

parseEntry = do
  name <- (T.pack <$>) $ P.manyTill (P.alphaNum P.<|> P.oneOf "_") $ P.try $ P.oneOf ":"
  P.spaces
  baseSets <- (map T.pack <$>) $ P.sepBy (P.many1 $ P.alphaNum P.<|> P.oneOf "_") (P.many1 $ P.oneOf " ")
  P.newline
  P.many P.newline
  P.string "Edges:" >> P.newline
  edges <- (M.fromListWith (++) <$>) $ P.many $ P.try $ do
    edgeName <- (T.pack <$>) $ P.manyTill (P.alphaNum P.<|> P.oneOf "_") P.space
    P.spaces
    targetName <- (T.pack <$>) $ P.manyTill (P.alphaNum P.<|> P.oneOf "_") $ P.newline
    return (edgeName,[targetName])
  P.many1 P.newline
  P.string "Data:" >> P.newline
  attrs <- (M.fromList <$>) $ P.many $ P.try $ do
    dataKey <- (T.pack <$>) $ P.manyTill (P.alphaNum P.<|> P.oneOf "_") $ P.char ':'
    P.newline
    dataData <- (T.pack <$>) $ P.manyTill (P.anyChar) $ P.try $ (P.newline >> P.newline >> return ()) P.<|> P.eof
    return (dataKey,dataData)
  return Entry{entName=name, entBaseSets=baseSets, entEdges=edges, entData=attrs}

safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (x:xs) = safeLast xs

allButLast [] = []
allButLast (x:[]) = []
allButLast (x:xs) = x:allButLast xs

listJoin x y = do
  xRow <- x
  lastVal <- maybeToList $ safeLast xRow
  pref <- return $ allButLast xRow
  yRow <- y
  case yRow of
    y:ys | y == lastVal -> return $ pref ++ ys
    _ -> []

listOneClosure x = go [] x x
  where
    go _ [] _ = []
    go univ base x
      = base ++
        go (univ++base)
           (filter (not . (`elem` (univ++base))) (listJoin base x))
           x

evalSetExp :: Eq v => SetExp T.Text -> Env v dat -> Maybe [[v]]
evalSetExp (AtomSet x) (Env vars _ _) = M.lookup x vars
evalSetExp NoneSet _ = Just []
evalSetExp UnivSet (Env _ univ _) = Just $ do
  x <- univ
  return [x]
evalSetExp IdentSet (Env _ univ _) = Just $ do
  x <- univ
  return [x,x]
evalSetExp (IsectSet x y) e
  = (\v -> filter (`elem` v)) <$> evalSetExp x e <*> evalSetExp y e
evalSetExp (UnionSet x y) e
  = (\x' y' -> x' ++ filter (not . (`elem` x')) y') <$> evalSetExp x e <*> evalSetExp y e
evalSetExp (DiffSet x y) e
  = (\x' y' -> filter (not . (`elem` y')) x') <$> evalSetExp x e <*> evalSetExp y e
evalSetExp (JoinSet x y) e = do
  x' <- evalSetExp x e
  y' <- evalSetExp y e
  return $ listJoin x' y'
evalSetExp (ProdSet x y) e = do
  x' <- evalSetExp x e
  y' <- evalSetExp y e
  return $ do { xv <- x'; yv <- y'; return $ xv ++ yv; }
evalSetExp (OneClosureSet x) e = do
  x' <- evalSetExp x e
  return $ listOneClosure x'
evalSetExp (ClosureSet x) e = evalSetExp (UnionSet IdentSet (OneClosureSet x)) e
evalSetExp (TransposeSet x) e = map reverse <$> evalSetExp x e
evalSetExp (FilterSet base rel) e = do
  base <- evalSetExp base e
  valName <- return $ freshEnvVar e
  flip filterM base $ \val ->
    evalRelExp (fromMaybe valName <$> rel) e{envVars=M.insert valName [val] (envVars e)}
evalSetExp (MapSet base rel) e = do
  base <- evalSetExp base e
  valName <- return $ freshEnvVar e
  ret <- flip mapM base $ \val ->
    evalSetExp (fromMaybe valName <$> rel) e{envVars=M.insert valName [val] (envVars e)}
  return $ nub $ join ret

evalRelExp :: Eq v => RelExp T.Text -> Env v dat -> Maybe Bool
evalRelExp (InRel x y) e = do
  x' <- evalSetExp x e
  y' <- evalSetExp y e
  return $ all (`elem` y') x'
evalRelExp (EqRel x y) e = evalRelExp (AndRel (InRel x y) (InRel y x)) e
evalRelExp (NeqRel x y) e = evalRelExp (NotRel (EqRel x y)) e
evalRelExp (SomeRel x) e = do
  x' <- evalSetExp x e
  return $ case x' of
    [] -> False
    _  -> True
evalRelExp (NoRel x) e = do
  x' <- evalSetExp x e
  return $ case x' of
    [] -> True
    _  -> False
evalRelExp (LoneRel x) e = do
  x' <- evalSetExp x e
  return $ case x' of
    [] -> True
    _:[] -> True
    _  -> False
evalRelExp (OneRel x) e = do
  x' <- evalSetExp x e
  return $ case x' of
    _:[] -> True
    _  -> False
evalRelExp (NotRel r) e = not <$> evalRelExp r e
evalRelExp (AndRel x y) e = (&&) <$> evalRelExp x e <*> evalRelExp y e
evalRelExp (OrRel x y) e = (||) <$> evalRelExp x e <*> evalRelExp y e
evalRelExp (ImplyRel x y) e = (\a b -> not a || b) <$> evalRelExp x e <*> evalRelExp y e
evalRelExp (IffRel x y) e = (==) <$> evalRelExp x e <*> evalRelExp y e
evalRelExp (QAllRel base rel) e = do
  base <- evalSetExp base e
  valName <- return $ freshEnvVar e
  (all id <$>) $ sequence $ base >>= \val ->
    return $ evalRelExp (fromMaybe valName <$> rel)
                        e{envVars=M.insert valName [val] (envVars e)}
evalRelExp (QSomeRel base rel) e = do
  base <- evalSetExp base e
  valName <- return $ freshEnvVar e
  (any id <$>) $ sequence $ base >>= \val ->
    return $ evalRelExp (fromMaybe valName <$> rel)
                        e{envVars=M.insert valName [val] (envVars e)}
evalRelExp (QOneRel base rel) e = do
  base <- evalSetExp base e
  valName <- return $ freshEnvVar e
  l <- fmap length $ filterM id $ base >>= \val ->
    return $ evalRelExp (fromMaybe valName <$> rel)
                        e{envVars=M.insert valName [val] (envVars e)}
  return $ l == 1
evalRelExp (QLoneRel base rel) e = do
  base <- evalSetExp base e
  valName <- return $ freshEnvVar e
  l <- fmap length $ filterM id $ base >>= \val ->
    return $ evalRelExp (fromMaybe valName <$> rel)
                        e{envVars=M.insert valName [val] (envVars e)}
  return $ l <= 1
evalRelExp (QNoRel base rel) e = do
  base <- evalSetExp base e
  valName <- return $ freshEnvVar e
  (not <$>) $ (any id <$>) $ sequence $ base >>= \val ->
    return $ evalRelExp (fromMaybe valName <$> rel)
                        e{envVars=M.insert valName [val] (envVars e)}

defaultEnv = Env
  { envVars = M.fromList []
  , envUniverse = []
  , envExtra = M.fromList []
  }

testEnv = Env
  { envVars = M.fromList
            [ ("this", [["goodbye"]])
            , ("that", [["stuff"]])
            , ("hello", [["hello"]])
            , ("left", [["hello", "goodbye"],["goodbye","sad"]])
            , ("right", [["sad","happy"]])
            ]
  , envUniverse = ["hello","goodbye","stuff","sad","happy"]
  , envExtra = M.fromList $ zip (envUniverse testEnv) (repeat ())
  }

getFiles = do
  currdir <- getCurrentDirectory
  contents <- getDirectoryContents currdir
  filter (not . isPrefixOf "." . takeBaseName) <$> filterM doesFileExist contents

whileEither_ :: Monad m => m (Either a b) -> (a -> m ()) -> (b -> m ()) -> m ()
whileEither_ cond lOp rOp = do
  cond' <- cond
  case cond' of
    Left lVal -> lOp lVal
    Right rVal -> rOp rVal >> whileEither_ cond lOp rOp

lineJoin [] = ""
lineJoin ls = foldl1 (\x y -> x <> "\n" <> y) ls

prefixWith s = lineJoin . map (s <>) . T.lines
indent = prefixWith "  "

graphmain :: IO ()
graphmain = do
  files <- getFiles
  -- mapM putStrLn files
  files <- sequence $ map (flip openFile ReadMode) files
  files <- sequence $ map hGetContents files
  entries <- return $ sequence $ map (P.parse parseEntry "") files
  putStrLn $ show entries
  (Right entries) <- return entries
  env <- return $ foldl (flip applyEntry) defaultEnv entries
  putStrLn "\n\n-----------------\n"
  -- putStrLn $ show env
  -- env <- return $ testEnv
  expLine <- return $
       P.parse (P.try (Left <$> (P.reserved alloyish "setexp" *> parseSetExp <* P.eof))
                P.<|> (Right <$> (P.reserved alloyish "relexp" *> parseRelExp <* P.eof)))
               "" <$> (putStr "> " >> hFlush stdout >> getLine)
  step <- return $ expLine >>= \case
    Left err -> putStrLn $ "parse error: " ++ show err
    Right x -> do
      putStrLn $ show x
      case x of
        Left x -> do
          x <- return $ T.pack <$> x
          putStrLn $ case evalSetExp x env of
            Nothing -> "Error"
            Just items -> T.unpack $ T.unlines $ do
              [item] <- items
              foldl1 (++) $ [
                return $ item <> ":"
                              <> (foldl (\x y -> x <> " " <> y) ""
                                        $ sort $ map fst $ filter (([item] `elem`) . snd)
                                        $ M.assocs $ envVars env)
                , do
                  edges <- return $ map (\ (k,v) -> (k,join v))
                                  $ filter ((>= 1) . length . snd)
                                  $ map (\ (k,v) -> (k, filter ((>= 1) . length) v))
                                  $ map (\ (k,v) -> (k,map tail $ filter (([item] `isPrefixOf`)) v))
                                  $ M.assocs $ envVars env
                  dat <- return $ (\ (k,v) -> [k <> ":", indent $ prefixWith "-> " v]) =<< (sort $ map (\ (k,v) -> (k, lineJoin v)) $ edges)
                  map indent dat
                , do
                  (Just dat) <- return $ M.lookup item $ envExtra env
                  dat <- return $ sort (M.assocs dat)
                                >>= \ (k,v) -> [k <> ":",
                                                indent v]
                  map indent dat
                ]
        Right x -> do
          x <- return $ T.pack <$> x
          putStrLn $ maybe "Error" show $ evalRelExp x env
  sequence_ $ repeat step

-- main = graphmain
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

