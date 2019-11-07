{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Data.List (nub)
import qualified Data.Map.Lazy as M

data SetExp v
  = BaseSet v
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
  deriving (Eq,Show,Read)

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
  deriving (Eq,Show,Read)

data Env v = Env
  { envVars     :: M.Map Text [[v]] -- relations
  , envUniverse :: [v]
  } deriving (Eq,Show,Read)

-- leftToMaybe (Left x) = Some x
-- leftToMaybe _ = Nothing

-- lookupClosure :: Ord k => M.Map k (Either k v) -> [[v]]
-- lookupClosure m = (rights $ map sequence $ M.elems m) ++
--                   (lookupClosure $
--                    M.mapMaybe (sequence $ map (either (M.lookup m) id)) $
--                    map (\ (k,v) -> either (k,) (k,) <$> v) $
--                    filter (any isLeft) $
--                    M.assocs m)

last [] = Nothing
last (x:[]) = Just x
last (x:xs) = last xs

allButLast [] = []
allButLast (x:[]) = []
allButLast (x:xs) = x:allButLast xs

listJoin x y = do
  xRow <- x
  lastVal <- maybeToList $ last xRow
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
           (filter (not . `elem` univ) (listJoin base x))
           x

evalSetExp :: SetExp Text -> Env v -> Maybe [[v]]
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
evalSetExp (Transpose x) e = map reverse $ evalSetExp x e

evalRelExp :: RelExp Text -> Env v -> Maybe Bool
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



main :: IO ()
main = do
  putStrLn $ show $ (
    lookupClosure $ M.fromList
      [ ("0",0)
      , ("1",["0"])
      , ("2",["0","1"])
      , ("3",["0","1","2"])
      ]
  )

