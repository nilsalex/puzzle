module Lib where

import qualified Data.Set as Set
import Data.Maybe
import Data.Tree

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Pos = (Int, Int, Int)
type Grid = Set.Set Pos
type Blocks = [Int]
data Game = Game Int Grid Pos Dir deriving (Show, Eq, Ord)
data Dir = U | D | L | R | F | B deriving (Show, Eq, Ord)

fromDir :: Dir -> (Int, Int, Int)
fromDir U = (0, 0, 1)
fromDir D = (0, 0, -1)
fromDir L = (-1, 0, 0)
fromDir R = (1, 0, 0)
fromDir F = (1, 0, 0)
fromDir B = (-1, 0, 0)

turns :: Dir -> [Dir]
turns U = [L, R, F, B]
turns D = [L, R, F, B]
turns L = [U, D, F, B]
turns R = [U, D, F, B]
turns B = [U, D, L, R]
turns F = [U, D, L, R]

blocks :: Blocks
blocks = [2, 2, 1, 2, 1, 1, 2, 3, 2, 3, 1, 1, 1, 2, 2, 1]

truncateTree :: Ord a => Int -> Tree a -> Tree a
truncateTree 0 (Node a _) = Node a []
truncateTree n (Node a f) = Node a $ map (truncateTree (n-1)) f

tree :: Tree Dir
tree = unfoldTree (\dir -> (dir, turns dir)) R

gameTree :: Blocks -> Game -> Tree Game
gameTree [] game = Node game []
gameTree (b:bs) game@(Game _ _ _ dir)  = Node game forest
    where
        dirs = turns dir
        gs = map (\dir -> moveN dir b game) dirs
        gs' = catMaybes gs
        forest = map (gameTree bs) gs'

addIntMaybe :: Int -> Int -> Int -> Maybe Int
addIntMaybe n a b = if s < n && s >= 0 then Just s else Nothing
    where s = a + b

addPosMaybe :: Int -> Pos -> Pos -> Maybe Pos
addPosMaybe n (p1, p2, p3) (q1, q2, q3) =
    do
     r1 <- addIntMaybe n p1 q1
     r2 <- addIntMaybe n p2 q2
     r3 <- addIntMaybe n p3 q3
     return (r1, r2, r3)

multPos :: Int -> Pos -> Pos
multPos a (p1, p2, p3) = (a*p1, a*p2, a*p3)

insertMaybe :: Ord a => a -> Set.Set a -> Maybe (Set.Set a)
insertMaybe a s = if size < size' then Just s' else Nothing
    where
        size = Set.size s
        size' = Set.size s'
        s' = Set.insert a s

newGame :: Int -> Game
newGame size = Game size (Set.empty) (0, 0, 0) D

moveN :: Dir -> Int -> Game -> Maybe Game
moveN dir n (Game size grid pos _) = 
    do
     let dirVec = fromDir dir
     let dirVec' = multPos n dirVec
     pos' <- addPosMaybe size pos dirVec'
     grid' <- insertMaybe pos' grid
     let game' = Game size grid' pos' dir
     return game'

full :: Game -> Bool
full (Game size grid _ _) = size*size*size == Set.size grid

findNode :: (a -> Bool) -> Tree a -> a
findNode p (Node n f)
    | p a = a
    | otherwise = 
