module Test.Main where

import Prelude

import Data.List (List(..), fromFoldable)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Main (CellState(..), CellState, countAliveNeighbours, gameStep)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "basic functionality" do
    it "empty game" do
      let newGame = gameStep Nil
      newGame `shouldEqual` Nil
    it "count alive neighbours of middle cell" do
      let neighbourCount = countAliveNeighbours 1 1 $ createGameGrid [
        [Dead, Alive, Dead],
        [Alive, Alive, Dead],
        [Dead, Alive, Alive]]
      neighbourCount `shouldEqual` 4
    it "count alive neighbours of edge cell" do
      let neighbourCount = countAliveNeighbours 2 2 $ createGameGrid [
        [Dead, Alive, Dead],
        [Alive, Alive, Dead],
        [Dead, Alive, Alive]]
      neighbourCount `shouldEqual` 2
  
  describe "game of life rules" do
    it "live cell with <2 live neighbours dies" do
      (gameStep $ createGameGrid [
        [Dead, Dead, Dead],
        [Alive, Alive, Dead],
        [Dead, Dead, Dead]]) `shouldEqual` 
          createGameGrid [
        [Dead, Dead, Dead],
        [Dead, Dead, Dead],
        [Dead, Dead, Dead]]
    it "live cell with 2 || 3 live neighbours survives" do
      (gameStep $ createGameGrid [
        [Alive, Dead, Dead],
        [Dead, Alive, Dead],
        [Dead, Dead, Alive]]) `shouldEqual` 
          createGameGrid [
        [Dead, Dead, Dead],
        [Dead, Alive, Dead],
        [Dead, Dead, Dead]]
    it "live cell with >3 neighbours dies" do
      (gameStep $ createGameGrid [
        [Alive, Dead, Alive],
        [Dead, Alive, Dead],
        [Alive, Dead, Alive]]) `shouldEqual` 
          createGameGrid [
        [Dead, Alive, Dead],
        [Alive, Dead, Alive],
        [Dead, Alive, Dead]]
    it "dead cell with 3 neighbours lives" do
      (gameStep $ createGameGrid [
        [Alive, Dead, Dead],
        [Dead, Dead, Dead],
        [Alive, Dead, Alive]]) `shouldEqual` 
          createGameGrid [
        [Dead, Dead, Dead],
        [Dead, Alive, Dead],
        [Dead, Dead, Dead]]

createGameGrid :: Array (Array CellState) -> List (List CellState)
createGameGrid = fromFoldable <<< map fromFoldable