module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.Map qualified as Map
import Data.Text (Text)
import Game.Board

import Network.WebSockets qualified as WS

main :: IO ()
main =
  let initBoard =
        Board . Map.fromList . map cellToEntry $
          [ ((3, 3), Red)
          , ((3, 4), Blue)
          , ((4, 3), Blue)
          , ((7, 8), Blue)
          , ((8, 8), Blue)
          , ((9, 8), Blue)
          ]
      boards = iterate advanceBoard initBoard
   in forM_
        boards
        ( \board -> do
            mapM_ putStrLn $ boardToLines 10 10 board
            putStrLn . replicate 10 $ '='
            threadDelay (1000 * 500) -- sleep for 500 ms
        )
type ClientName = Text

type Client = (ClientName, WS.Connection)

data ServerState = SState
  { client1 :: !(Maybe Client) -- player 1
  , client2 :: !(Maybe Client) -- player 2
  }

cellToEntry :: ((Int, Int), Color) -> (Coordinate, Cell)
cellToEntry ((x, y), color) = (coord, Cell color coord)
 where
  coord = Coordinate (x, y)

boardToLines :: Int -> Int -> Board -> [String]
boardToLines xs ys (Board bMap) =
  [ [ charIcon . (`Map.lookup` bMap) $ Coordinate (x, y)
    | x <- [0 .. xs]
    ]
  | y <- [0 .. ys]
  ]

charIcon :: Maybe Cell -> Char
charIcon Nothing = ' '
charIcon (Just (Cell color _)) = case color of
  Red -> 'o'
  Blue -> 'x'
  Wall -> '#'
