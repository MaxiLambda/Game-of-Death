{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Game

import qualified Network.WebSockets as WS

main :: IO ()
main = putStrLn "Not implemented"

type ClientName = Text

type Client = (ClientName, WS.Connection)


data ServerState = SState {
  client1 :: !(Maybe Client), --player 1
  client2 :: !(Maybe Client), --player 2
  rounds :: !Int, --the number of rounds to play until the game ends
  steps :: ![Int], --the number of steps for each round
  currentRound :: !Int, --the current round
  board :: !Board --the board state
}
