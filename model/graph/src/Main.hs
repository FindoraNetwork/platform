{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Data.List (nub)
import qualified Data.Map.Lazy as M

data SetExp v
  = BaseSet v
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
  deriving (Eq,Show,Read)

data Env v = Env
  { envVars  :: M.Map Text [[v]] -- relations
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

-- listOneClosure x = do
  
--   xRow <- x
--   lastVal <- maybeToList $ last xRow
--   pref <- return $ allButLast xRow
--   yRow <- y
--   case yRow of
--     y:ys | y == lastVal -> return $ pref ++ ys
--     _ -> []

evalSetExp :: SetExp Text -> Env v -> Maybe [[v]]
evalSetExp (AtomSet x) (Env vars) = M.lookup x vars
evalSetExp (IsectSet x y) e
  = (\v -> filter (`elem` v)) <$> evalSetExp x e <*> evalSetExp y e
evalSetExp (UnionSet x y) e
  = (\x' y' -> x' ++ filter (not . (`elem` x')) y') <$> evalSetExp x e <*> evalSetExp y e
evalSetExp (DiffSet x y) e
  = (\x' y' -> filter (not . (`elem` y')) x') <$> evalSetExp x e <*> evalSetExp y e
evalSetExp (AppSet x y) e = do
  x' <- evalSetExp x e
  y' <- evalSetExp y e
  return $ listJoint x' y'
evalSetExp (OneClosureSet x y) e = do

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

