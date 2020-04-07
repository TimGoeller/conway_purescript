module Main where

import Prelude

import Control.MonadZero (guard)
import Data.List (List(..), foldl, length, (!!), (..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)

data CellState = Dead | Alive 

gameStep :: List (List CellState) -> List (List CellState)
gameStep Nil = Nil
gameStep cellStates = do
  currentRow <- (0) .. (length cellStates - 1)
  pure do
    currentColumn <- (0) .. ((length $ fromMaybe Nil (cellStates !! currentRow)) - 1)
    case (cellStates !! currentRow) >>= (_ !! currentColumn) of
      Nothing -> Nil
      Just cellState ->
        pure $ transformCell cellState (countAliveNeighbours currentRow currentColumn cellStates) where
          transformCell :: CellState -> Int -> CellState
          transformCell Dead neighbours | neighbours == 3 = Alive
          transformCell Alive neighbours | neighbours == 3 || neighbours == 2 = Alive
          transformCell _ _ = Dead

countAliveNeighbours :: Int -> Int -> List (List CellState) -> Int
countAliveNeighbours row column cellStates = foldl (\b a -> b + boolToInt a) 0 do 
  currentRow    <-  (row - 1) .. (row + 1)
  currentColumn <-  (column - 1) .. (column + 1)
  guard (not ((currentRow == row) && (currentColumn == column)))
  pure $ isCellAlive currentRow currentColumn where
    isCellAlive :: Int -> Int -> Boolean
    isCellAlive neighbourRow neighbourColumn = case 
    (cellStates !! neighbourRow) >>= (_ !! neighbourColumn) of
      Just a -> a == Alive
      Nothing -> false

boolToInt :: Boolean -> Int 
boolToInt bool = if bool == true then 1 else 0

instance showCell :: Show CellState where
  show Dead = "Dead"
  show Alive = "Alive"  

derive instance eqCell :: Eq CellState

main :: Effect Unit
main = do 
  log "test"
