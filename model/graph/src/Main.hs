{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor     #-}
module Main where
import qualified Data.Text as T
import           Data.List (nub)
import qualified Data.Map.Lazy as M
import qualified Text.Parsec   as P
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language (javaStyle)
import           Data.Maybe (maybeToList)
import           Control.Applicative ((<$>),(<*>),(*>))

alloyish = P.makeTokenParser $ javaStyle
  { P.reservedNames = [ "sig", "abstract", "extends"
                    , "in", "let", "fun", "pred", "fact", "check"
                    , "all", "one", "some", "no", "sum"
                    , "none", "univ", "ident"
                    , "fresh", "setexp", "relexp"
                    ]
  , P.caseSensitive = True
  }

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

parseRelExp
  = foldl1 (\x y -> P.try x P.<|> y) $
  [ uncurry AndRel <$> relBop "&&"
  , uncurry OrRel <$> relBop "||"
  , uncurry ImplyRel <$> relBop "=>"
  , uncurry IffRel <$> relBop "<=>"
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
  deriving (Eq,Show,Read,Functor)

data Env v = Env
  { envVars     :: M.Map T.Text [[v]] -- relations
  , envUniverse :: [v]
  } deriving (Eq,Show,Read,Functor)

-- leftToMaybe (Left x) = Some x
-- leftToMaybe _ = Nothing

-- lookupClosure :: Ord k => M.Map k (Either k v) -> [[v]]
-- lookupClosure m = (rights $ map sequence $ M.elems m) ++
--                   (lookupClosure $
--                    M.mapMaybe (sequence $ map (either (M.lookup m) id)) $
--                    map (\ (k,v) -> either (k,) (k,) <$> v) $
--                    filter (any isLeft) $
--                    M.assocs m)

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

evalSetExp :: Eq v => SetExp T.Text -> Env v -> Maybe [[v]]
evalSetExp (AtomSet x) (Env vars _) = M.lookup x vars
evalSetExp NoneSet _ = Just []
evalSetExp UnivSet (Env _ univ) = Just $ do
  x <- univ
  return [x]
evalSetExp IdentSet (Env _ univ) = Just $ do
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

evalRelExp :: Eq v => RelExp T.Text -> Env v -> Maybe Bool
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

defaultEnv = Env
  { envVars = M.fromList
            [ ("this", [["goodbye"]])
            , ("that", [["stuff"]])
            , ("hello", [["hello"]])
            , ("left", [["hello", "goodbye"],["goodbye","sad"]])
            , ("right", [["sad","happy"]])
            ]
  , envUniverse = ["hello","goodbye","stuff","sad","happy"]
  }

main :: IO ()
main = do
  x <- P.parse (P.try (Left <$> (P.reserved alloyish "setexp" *> parseSetExp <* P.eof))
                P.<|> (Right <$> (P.reserved alloyish "relexp" *> parseRelExp <* P.eof)))
               "" <$> getLine
  (Right x) <- return x
  putStrLn $ show x
  case x of
    Left x -> do
      x <- return $ T.pack <$> x
      putStrLn $ show $ evalSetExp x defaultEnv
    Right x -> do
      x <- return $ T.pack <$> x
      putStrLn $ show $ evalRelExp x defaultEnv

