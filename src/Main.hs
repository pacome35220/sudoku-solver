module Main where

import System.Environment
import System.Exit

import qualified Data.Char as Char
import qualified Data.List.Split

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
readGrid str = fmap callbackRow (Data.List.Split.chunksOf 9 str)
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
        showCell (Fixed x) = show x
        showCell _ = "."

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
            putStrLn (showGrid (readGrid grid))
