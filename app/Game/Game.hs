{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Game.Game (
)
where

import Game.Board (Board)
import Control.Monad.State (State)


data Config = Config {
  rounds :: !Int,
  stepsPerRound :: !(Int -> Int),
  length :: !Int, -- cells can only exists between a width of 0 and lenght
  height :: !Int, -- cells can only exists with a height between 0 and height
  cellsPerRound :: !(Int -> Int) 
}

data RuntimeConfig = RuntimeConfig {
  config :: !Config,
  currentRound :: !Int,
  currentStep :: !Int
}

-- >>> 1 + 1 
data GameState = Game {
  board :: !Board,
  runtimeConfig :: !RuntimeConfig
}

-- trying to use advanceStep as a StateProcessor returning the next step number
--advanceStep :: State GameState Int
--advanceStep = (\game -> )


