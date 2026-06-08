module Main (main) where

import Control.Monad (when)
import Control.Monad.State (State, runState, state)
import Data.List (intersperse, nub)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Random (StdGen, mkStdGen, random, randomR)

type Point = (Int, Int)

generateMixed :: Int -> StdGen -> (([Point], Int), StdGen)
generateMixed m = runState $ do
    coin <- state random
    if coin then genHardSolvable m else return (genHardUnsolvable m)

genHardSolvable :: Int -> State StdGen ([Point], Int)
genHardSolvable m = go
  where
    n = max 3 (m `div` 2 + 1)
    b = max 2 (ceiling (sqrt (fromIntegral m :: Double)))
    go = do
        x <- state $ randomR (-b, b)
        y <- state $ randomR (-b, b)
        poly <- buildDensePolyline n b (x, y)
        let uniquePts = nub (pointsOnPolyline poly)
        if length uniquePts >= m
            then do
                shuffled <- shuffleListRandomly (take m uniquePts)
                return (shuffled, n)
            else go

buildDensePolyline :: Int -> Int -> Point -> State StdGen [Point]
buildDensePolyline 0 _ s = return [s]
buildDensePolyline n b s@(sx, sy) = do
    let dirs = [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)]
    i <- state $ randomR (0, 7)
    len <- state $ randomR (1, b * 2)
    let (dx, dy) = dirs !! i
        nxt = (sx + dx * len, sy + dy * len)

    if fst nxt >= (-b) && fst nxt <= b && snd nxt >= (-b) && snd nxt <= b
        then (s :) <$> buildDensePolyline (n - 1) b nxt
        else buildDensePolyline n b s

genHardUnsolvable :: Int -> ([Point], Int)
genHardUnsolvable m =
    let r = 20.0 :: Double
        pts =
            nub
                [ (round (r * cos t), round (r * sin t))
                | i <- [0 .. m - 1]
                , let t = 2 * pi * fromIntegral i / fromIntegral m
                ]
        n = (length pts + 1) `div` 2 - 1
     in (pts, max 1 n)

pointsOnPolyline :: [Point] -> [Point]
pointsOnPolyline [] = []
pointsOnPolyline [p] = [p]
pointsOnPolyline (p1 : p2 : ps) = p1 : interiorPoints p1 p2 ++ pointsOnPolyline (p2 : ps)

interiorPoints :: Point -> Point -> [Point]
interiorPoints (x1, y1) (x2, y2) =
    let dx = signum (x2 - x1); dy = signum (y2 - y1)
     in [(x1 + dx * i, y1 + dy * i) | i <- [1 .. max (abs (x2 - x1)) (abs (y2 - y1)) - 1]]

shuffleListRandomly :: [a] -> State StdGen [a]
shuffleListRandomly [] = return []
shuffleListRandomly xs = do
    i <- state $ randomR (0, length xs - 1)
    let (ys, zs) = splitAt i xs
    case zs of
        z : rest -> (z :) <$> shuffleListRandomly (ys ++ rest)
        [] -> error "Impossible: index out of bounds"

renderPlane :: ([Point], Int) -> String
renderPlane (pts, n) =
    unlines $ (show n ++ " " ++ show (length pts))
              : map (\(x, y) -> unwords [show x, show y]) pts

main :: IO ()
main = do
    args <- getArgs
    let ms = map read args :: [Int]
    when (null ms) $ do
        putStrLn "usage: generators-exe M [M..]"
        exitFailure
    let seed = 0 :: Int
        blocks =
            [ renderPlane inner
            | m <- ms
            , let ((pts, n), _) = generateMixed m (mkStdGen (seed + m * 31337))
                  inner = (pts, n)
            ]
        content = unlines (intersperse "" blocks)
    writeFile "plane.txt" content
    putStrLn $ "wrote " ++ show (length ms) ++ " plane(s) to plane.txt"
