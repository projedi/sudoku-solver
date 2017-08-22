module Lib (someFunc) where

import qualified Data.List as List
import Data.List ((\\))
import qualified Data.Maybe as Maybe

type Value = Int
type Position = (Int, Int)
data CellData
  = CellValue Value
  | CellAvailability [Value]
  deriving (Eq)
type Cell = (Position, CellData)
type Grid = [Cell]

horizontalContextFor :: Grid -> Position -> [Cell]
horizontalContextFor grid (x,y) = [cell | cell@((xc,yc),_) <- grid, xc == x, yc /= y]

verticalContextFor :: Grid -> Position -> [Cell]
verticalContextFor grid (x,y) = [cell | cell@((xc,yc),_) <- grid, xc /= x, yc == y]

cellContextFor :: Grid -> Position -> [Cell]
cellContextFor grid (x,y) = [cell | cell@((xc,yc),_) <- grid, (yc /= y || xc /= x), (x `div` 3 == xc `div` 3), (y `div` 3 == yc `div` 3)]

valuesInCells :: [Cell] -> [Value]
valuesInCells = Maybe.catMaybes . map go
 where go :: Cell -> Maybe Value
       go (_,CellValue v) = Just v
       go (_,CellAvailability _) = Nothing

availableForCell :: Grid -> Position -> [Value]
availableForCell grid pos =
  let completeContext = (horizontalContextFor grid pos) `List.union` (verticalContextFor grid pos) `List.union` (cellContextFor grid pos)
  in [1..9] \\ (valuesInCells completeContext)

updateAvailableForCell :: Grid -> Grid
updateAvailableForCell grid = map go grid
 where go :: Cell -> Cell
       go c@(_,CellValue _) = c
       go (p, CellAvailability _) = (p, CellAvailability $ availableForCell grid p)

printGrid :: Grid -> String
printGrid = unlines . map (concatMap go) . List.groupBy (\((_,pl),_) ((_,pr),_) -> pl == pr)
 where go :: Cell -> String
       go (_, CellValue v) = show v
       go (_, CellAvailability _) = "."

printAvailability :: Grid -> String
printAvailability = unlines . filter (not . null) . map go
 where go :: Cell -> String
       go (_, CellValue _) = ""
       go (p, CellAvailability vals) = show p ++ ": " ++ show vals

readGrid :: String -> Grid
readGrid = concat . zipWith (\i -> zipWith (\j c -> ((j, i), go c)) [0..]) [0..] . lines
 where go :: Char -> CellData
       go '.' = CellAvailability [1..9]
       go c
        | c `elem` ['1'..'9'] = CellValue (read [c])
        | otherwise = error "Invalid grid"

initialGrid :: String
initialGrid = unlines
  [ "64..3...7"
  , "5.1.7.9.."
  , ".......1."
  , "..49.8.6."
  , ".8...3.2."
  , "...4....."
  , "4..157.3."
  , "2.83...4."
  , "75.....96"
  ]

someFunc :: IO ()
someFunc = do
  let grid = readGrid initialGrid
  putStrLn $ printGrid grid
  putStrLn $ printAvailability grid
  putStrLn "Update available for cell"
  let updatedGrid = updateAvailableForCell grid
  putStrLn $ printGrid updatedGrid
  putStrLn $ printAvailability updatedGrid
