module Main where

import System.Environment
import System.Exit

import Data.List

type Grid             = Matrix Value

type Matrix a         = [Row a]

type Row a            = [a]

type Value            = Char

boxsize               :: Int
boxsize               =  3

values                :: [Value]
values                =  ['1'..'9']

empty                 :: Value -> Bool
empty                 =  (== '.')

single                :: [a] -> Bool
single [_]            =  True
single _              =  False

rows                  :: Matrix a -> [Row a]
rows                  =  id

cols                  :: Matrix a -> [Row a]
cols                  =  transpose

boxs                  :: Matrix a -> [Row a]
boxs                  =  unpack . map cols . pack
                         where
                            pack   = split . map split
                            split  = chop boxsize
                            unpack = map concat . concat

chop                  :: Int -> [a] -> [[a]]
chop _ []             =  []
chop n xs             =  take n xs : chop n (drop n xs)

valid                 :: Grid -> Bool
valid g               =  all nodups (rows g) &&
                         all nodups (cols g) &&
                         all nodups (boxs g)

nodups                :: Eq a => [a] -> Bool
nodups []             =  True
nodups (x:xs)         =  not (elem x xs) && nodups xs

type Choices          =  [Value]

choices               :: Grid -> Matrix Choices
choices               =  map (map choice)
                         where
                            choice v = if empty v then values else [v]

cp                    :: [[a]] -> [[a]]
cp []                 =  [[]]
cp (xs:xss)           =  [y:ys | y <- xs, ys <- cp xss]

collapse              :: Matrix [a] -> [Matrix a]
collapse              =  cp . map cp

prune                 :: Matrix Choices -> Matrix Choices
prune                 =  pruneBy boxs . pruneBy cols . pruneBy rows
                         where pruneBy f = f . map reduce . f

reduce                :: Row Choices -> Row Choices
reduce xss            =  [xs `minus` singles | xs <- xss]
                         where singles = concat (filter single xss)

minus                 :: Choices -> Choices -> Choices
xs `minus` ys         =  if single xs then xs else xs \\ ys

fix                   :: Eq a => (a -> a) -> a -> a
fix f x               =  if x == x' then x else fix f x'
                         where x' = f x

complete              :: Matrix Choices -> Bool
complete              =  all (all single)

void                  :: Matrix Choices -> Bool
void                  =  any (any null)

safe                  :: Matrix Choices -> Bool
safe cm               =  all consistent (rows cm) &&
                         all consistent (cols cm) &&
                         all consistent (boxs cm)

consistent            :: Row Choices -> Bool
consistent            =  nodups . concat . filter single

blocked               :: Matrix Choices -> Bool
blocked m             =  void m || not (safe m)

solve                :: Grid -> [Grid]
solve                = search . prune . choices

search                :: Matrix Choices -> [Grid]
search m
 | blocked m          =  []
 | complete m         =  collapse m
 | otherwise          =  [g | m' <- expand m
                            , g  <- search (prune m')]

expand                :: Matrix Choices -> [Matrix Choices]
expand m              =
      [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
      where
         (rows1,row:rows2) = break (any (not . single)) m
         (row1,cs:row2)    = break (not . single) row

toPrintable           :: [Grid] -> String
toPrintable           =  unlines . head

parseArgument         :: [String] -> Maybe String
parseArgument         ["--file", file] = Just file
parseArgument         ["-f", file] = Just file
parseArgument         _ = Nothing

main                  :: IO ()
main                  =  do
                     args <- getArgs
                     let mbFile = parseArgument args
                     case mbFile of
                        Nothing -> exitWith (ExitFailure 84)
                        Just file -> do
                           string <- readFile file
                           putStr (toPrintable (solve (lines string)))
