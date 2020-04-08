{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-} -- TODO: check if I need this
{-# LANGUAGE RankNTypes        #-}
module PolicyScript where
import qualified Data.Text as T
import qualified Text.PrettyPrint.Annotated as PP
import           Data.Ratio(numerator,denominator)
-- import           Debug.Trace(trace)

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


