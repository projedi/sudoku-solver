module Lib (someFunc, solve) where

import Control.Applicative ((<|>))
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

uniqueAvailableAmong :: Grid -> Cell -> [Cell] -> Maybe Value
uniqueAvailableAmong _ (_,CellValue v) _ = Just v
uniqueAvailableAmong grid (_,CellAvailability vs) cells =
  let otherVs = concat $ Maybe.catMaybes $ map go cells
      diffVs = vs \\ otherVs
  in case diffVs of
       [v] -> Just v
       _ -> Nothing
 where go :: Cell -> Maybe [Value]
       go (_,CellValue _) = Nothing
       go (_,CellAvailability vs) = Just vs

updateUniqueCell :: Grid -> Cell -> Cell
updateUniqueCell grid cell@(pos,_) =
  let horVal = uniqueAvailableAmong grid cell (horizontalContextFor grid pos)
      verVal = uniqueAvailableAmong grid cell (verticalContextFor grid pos)
      cellVal = uniqueAvailableAmong grid cell (cellContextFor grid pos)
  in case horVal <|> verVal <|> cellVal of
       Nothing -> cell
       Just v -> (pos, CellValue v)

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

updateUniqueInContexts :: Grid -> Grid
updateUniqueInContexts grid = map (updateUniqueCell grid) grid

update :: Grid -> Grid
update grid =
  let grid' = updateAvailableForCell grid
      grid'' = updateUniqueAvailability grid'
      grid''' = updateUniqueInContexts grid'
  in if grid' /= grid''
      then update grid''
      else if grid' /= grid'''
            then update grid'''
            else grid'

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

gridAsHTML :: Grid -> String
gridAsHTML grid = "<html><style>\
\ td.partial td {\
\   font-weight: lighter;\
\   font-size: xx-small;\
\ }\
\ td.full, td.partial {\
\   text-align: center;\
\   border: 1px solid;\
\ }\
\ </style><body><table style=\"border-collapse: collapse\"><tbody>"
  ++ go grid ++ "</tbody></table></body></html>"
 where go :: Grid -> String
       go = concatMap (makeRows . concatMap go') . List.groupBy (\((_,pl),_) ((_,pr),_) -> pl == pr)
       makeRows :: String -> String
       makeRows content = "<tr>" ++ content ++ "</tr>"
       go' :: Cell -> String
       go' (_, CellValue v) = "<td class=\"full\">" ++ show v ++ "</td>"
       go' (_, CellAvailability vals) = "<td class=\"partial\"><table><tbody>" ++ goPartial vals ++ "</tbody></table></td>"
       goPartial :: [Value] -> String
       goPartial vals = concatMap (\x -> "<tr>" ++ x ++ "</tr>")
                      $ map (concatMap (\x -> "<td>" ++ x ++ "</td>"))
                      $ map (map (\x -> if x `elem` vals then show x else "&nbsp;")) [[1,2,3],[4,5,6],[7,8,9]]

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
  [ ".1..7...5"
  , "6...5..4."
  , ".5..3...."
  , "..8..6..2"
  , "........7"
  , "..91.3..."
  , "........."
  , "..6..41.."
  , "8....53.4"
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
  putStrLn "Writing to /tmp/grid.html"
  writeFile "/tmp/grid.html" (gridAsHTML updatedGrid)
