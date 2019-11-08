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
import           Control.Monad (join, filterM)
import           System.Directory (getCurrentDirectory, getDirectoryContents, doesFileExist)
import           System.IO (hGetContents,openFile,IOMode(..))

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

data Env v dat = Env
  { envVars     :: M.Map T.Text [[v]] -- relations
  , envUniverse :: [v]
  , envExtra    :: M.Map v dat
  } deriving (Eq,Show,Read,Functor)

data Entry = Entry
  { entName  :: T.Text
  , entEdges :: M.Map T.Text [T.Text]
  , entData  :: M.Map T.Text T.Text
  } deriving (Eq,Show,Read)

applyEntry :: Entry -> Env T.Text (M.Map T.Text T.Text) -> Env T.Text (M.Map T.Text T.Text)
applyEntry (Entry name edges dat)
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
    edgeUpdates = do
      (k,v) <- M.assocs edges
      other <- v
      return $ flip M.alter k $ Just . \case
        Nothing -> [[name,other]]
        Just es -> if [name,other] `elem` es
          then es else [name,other]:es

parseEntry = do
  name <- (T.pack <$>) $ P.manyTill (P.alphaNum P.<|> P.oneOf "_") $ P.try $ P.newline
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
    P.spaces
    P.newline
    dataData <- (T.pack <$>) $ P.manyTill (P.anyChar) $ P.try $ (P.newline >> P.newline >> return ()) P.<|> P.eof
    return (dataKey,dataData)
  return Entry{entName=name, entEdges=edges, entData=attrs}

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
  filterM doesFileExist contents

main :: IO ()
main = do
  files <- getFiles
  -- mapM putStrLn files
  files <- sequence $ map (flip openFile ReadMode) files
  files <- sequence $ map hGetContents files
  entries <- return $ sequence $ map (P.parse parseEntry "") files
  -- putStrLn $ show entries
  (Right entries) <- return entries
  env <- return $ foldl (flip applyEntry) defaultEnv entries
  -- env <- return $ testEnv
  x <- P.parse (P.try (Left <$> (P.reserved alloyish "setexp" *> parseSetExp <* P.eof))
                P.<|> (Right <$> (P.reserved alloyish "relexp" *> parseRelExp <* P.eof)))
               "" <$> getLine
  (Right x) <- return x
  putStrLn $ show x
  case x of
    Left x -> do
      x <- return $ T.pack <$> x
      putStrLn $ show $ evalSetExp x env
    Right x -> do
      x <- return $ T.pack <$> x
      putStrLn $ show $ evalRelExp x env

