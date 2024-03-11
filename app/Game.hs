{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Game (
  Coordinate,
  Board
)
where

import Control.Monad.State (State)

data Color = Red | Blue | Black
newtype Colored a = Colored Color

newtype Coordinate = Coordinate (Int, Int)

data Cell = Cell {
  color :: !(Colored Cell),
  coordinate :: !Coordinate
}

newtype Board = Board [Cell]

data Config = Config {
  rounds :: !Int,
  stepsPerRound :: !(Int -> Int),
  length :: !Int, -- cells can only exists betwee a width of 0 and lenght
  height :: !Int, -- cells can only exists with a height between 0 and height
  cellsPerRound :: !(Int -> Int) 
}

data RuntimeConfig = RuntimeConfig {
  config :: !Config,
  currentRound :: !Int,
  currentStep :: !Int
}

data GameState = Game {
  board :: !Board,
  runtimeConfig :: !RuntimeConfig
}

-- trqing to use advanceStep as a StateProcessor returning the next step number
advanceStep :: State GameState Int
advanceStep = (\game -> )
