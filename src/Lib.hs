module Lib (someFunc, solve) where

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

updateUniqueAvailability :: Grid -> Grid
updateUniqueAvailability = map go
 where go :: Cell -> Cell
       go (p, CellAvailability [v]) = (p, CellValue v)
       go c = c

update :: Grid -> Grid
update grid =
  let grid' = updateAvailableForCell grid
      grid'' = updateUniqueAvailability grid'
  in if grid' == grid'' then grid'' else update grid''

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

solve :: String -> String
solve = printGrid . update . readGrid

initialGrid :: String
initialGrid = unlines
  [ ".1....43."
  , "7........"
  , "...2549.."
  , "17..4.2.6"
  , "....9...3"
  , "..3..6.8."
  , "..147..6."
  , "...5.812."
  , ".9..6.3.4"
  ]

someFunc :: IO ()
someFunc = do
  let grid = readGrid initialGrid
  putStrLn $ printGrid grid
  putStrLn $ printAvailability grid
  putStrLn "Updated grid"
  let updatedGrid = update grid
  putStrLn $ printGrid updatedGrid
  putStrLn $ printAvailability updatedGrid
