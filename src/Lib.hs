module Lib (someFunc) where

type Value = Int
data CellData
  = CellValue Value
  | CellAvailability [Value]
type Grid = [[CellData]]
type Position = (Int, Int)
type Cell = (Position, CellData)

printGrid :: Grid -> String
printGrid = unlines . map (concatMap go)
 where go :: CellData -> String
       go (CellValue v) = show v
       go (CellAvailability _) = "."

readGrid :: String -> Grid
readGrid = map (map go) . lines
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
