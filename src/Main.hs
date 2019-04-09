module Main where

import System.Environment
import System.Exit

import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.List as List

data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)
type Row  = [Cell]
type Grid = [Row]

parseArgument :: [String] -> Maybe String
parseArgument ["--grid", grid] = Just grid
parseArgument ["-g", grid] = Just grid
parseArgument _ = Nothing

readChar :: Char -> Bool
readChar '.' = True
readChar c
    | c `elem` ['1'..'9'] = True
    | otherwise = False

checkGrid :: String -> Bool
checkGrid "" = False
checkGrid str = all readChar str

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> [a] -> [b] -- où f = []
-- fmap :: (a -> b) -> Maybe a -> Maybe b -- où f = Maybe

readGrid :: String -> Grid
readGrid str = fmap callbackRow (Split.chunksOf 9 str)
    where
        callbackRow :: String -> Row
        callbackRow = fmap callbackCell
            where
                callbackCell :: Char -> Cell
                callbackCell '.' = Possible [1..9]
                callbackCell n = Fixed (Char.digitToInt n)

showGrid :: Grid -> String
showGrid grid = unlines ( map (\cells -> unwords (map showCell cells)) grid )
    where
        showCell :: Cell -> String
        showCell (Fixed x) = show x
        showCell _ = "."

showGridWithPossibilities :: Grid -> String
showGridWithPossibilities grid = unlines ( map (\cells -> unwords (map showCell cells)) grid)
    where
        showCell :: Cell -> String
        showCell (Fixed x) = show x ++ "          "
        showCell (Possible intTab) =
            "[" ++ List.foldl (\acc cell -> acc ++ findUnique cell) "" [1..9] ++ "]"
            where
                findUnique :: Int -> String
                findUnique x = case x `elem` intTab of
                    True -> show x
                    False -> " "

pruneRow :: Row -> Maybe Row
pruneRow row = traverse pruneCell row
    where
        fixeds = [x | Fixed x <- row]
        pruneCell :: Cell -> Maybe Cell
        pruneCell (Possible xs) = case xs List.\\ fixeds of
            []  -> Nothing
            [y] -> Just (Fixed y)
            ys  -> Just (Possible ys)
        pruneCell x = Just x

subGridsToRows :: Grid -> Grid
subGridsToRows =
    concatMap (\rows -> let [r1, r2, r3] = map (Split.chunksOf 3) rows
    in zipWith3 (\a b c -> a ++ b ++ c) r1 r2 r3)
    -- [] -> return ()
    . Split.chunksOf 3

main :: IO ()
main = do
    args <- getArgs
    -- print args
    let mbGrid = parseArgument args
    case mbGrid of
        Nothing -> exitWith (ExitFailure 84)
        Just grid -> do
            let len = length grid :: Int
            case len == 81 of
                True -> return ()
                False -> exitWith (ExitFailure 84)
            case checkGrid grid /= False of
                True -> return ()
                False -> exitWith (ExitFailure 84)
            let pruner = fmap List.transpose . traverse pruneRow . List.transpose
            let mbPrunedGrid = pruner (readGrid grid)
            case mbPrunedGrid of
                Nothing -> exitWith (ExitFailure 84)
                Just prunedGrid -> do
                    putStrLn (showGrid prunedGrid)
                    let fullPruner = fmap subGridsToRows . traverse pruneRow . subGridsToRows
                    let mbFullPrunedGrid = fullPruner prunedGrid
                    case mbFullPrunedGrid of
                        Nothing -> exitWith (ExitFailure 84)
                        Just fullPrunedGrid -> do
                            putStrLn (showGridWithPossibilities fullPrunedGrid)
