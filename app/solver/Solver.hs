module Main (main) where

import Control.DeepSeq (deepseq)
import Control.Exception (IOException, try)
import Control.Monad (forM, when)
import Control.Parallel.Strategies (rpar, runEval)
import Data.Foldable (asum)
import Data.List (sortOn, tails)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.Printf (printf)

type Point = (Int, Int)
type Line = (Int, Int, Int)

validSegment :: Point -> Point -> Bool
validSegment (x1, y1) (x2, y2) =
    let dx = abs (x2 - x1); dy = abs (y2 - y1)
     in dx == 0 || dy == 0 || dx == dy

pointOnSegment :: Point -> Point -> Point -> Bool
pointOnSegment (x1, y1) (x2, y2) (px, py) =
    min x1 x2 <= px
        && px <= max x1 x2
        && min y1 y2 <= py
        && py <= max y1 y2
        && (x2 - x1) * (py - y1) == (y2 - y1) * (px - x1)

coveredBySegment :: Point -> Point -> Set.Set Point -> Set.Set Point
coveredBySegment p1 p2 = Set.filter (pointOnSegment p1 p2)

pointLines :: Point -> [Line]
pointLines (x, y) = [(0, 1, y), (1, 0, x), (1, -1, x - y), (1, 1, x + y)]

lineIntersection :: Line -> Line -> Maybe Point
lineIntersection (a1, b1, c1) (a2, b2, c2) =
    let det = a1 * b2 - a2 * b1
        xn = c1 * b2 - c2 * b1
        yn = a1 * c2 - a2 * c1
     in if det /= 0 && xn `mod` det == 0 && yn `mod` det == 0
            then Just (xn `div` det, yn `div` det)
            else Nothing

generateCandidates :: [Point] -> [Point]
generateCandidates pts = Set.toList . Set.fromList $ pts ++ extra
  where
    ls = Set.toList . Set.fromList $ concatMap pointLines pts
    extra = [p | (l1 : ls') <- tails ls, l2 <- ls', Just p <- [lineIntersection l1 l2]]

nextMoves :: [Point] -> Point -> Set.Set Point -> [Point]
nextMoves candidates cur remPts =
    sortOn
        (Down . \cand -> Set.size (coveredBySegment cur cand remPts))
        [ v
        | v <- candidates
        , v /= cur
        , validSegment cur v
        , not (Set.null (coveredBySegment cur v remPts))
        ]

searchPure :: [Point] -> Point -> Set.Set Point -> [Point] -> Int -> Maybe [Point]
searchPure cands cur remPts path segs
    | Set.null remPts = Just (reverse path)
    | segs <= 0 = Nothing
    | otherwise =
        asum
            [ let newRem = Set.difference remPts (coveredBySegment cur v remPts)
               in newRem `deepseq` searchPure cands v newRem (v : path) (segs - 1)
            | v <- nextMoves cands cur remPts
            ]

solve :: Int -> [Point] -> [Point] -> Maybe [Point]
solve n cands pts =
    let ps = Set.fromList pts
     in asum [searchPure cands s (Set.delete s ps) [s] n | s <- Set.toList ps]

solveParallel :: Int -> [Point] -> [Point] -> Maybe [Point]
solveParallel n cands pts =
    let ps = Set.fromList pts
        tasks =
            [ newRem `deepseq` searchPure cands v newRem [v, s] (n - 1)
            | s <- Set.toList ps
            , let remPts = Set.delete s ps
            , v <- nextMoves cands s remPts
            , let newRem = Set.difference remPts (coveredBySegment s v remPts)
            ]
     in if Set.size ps <= 1
            then solve n cands pts
            else runEval (asum <$> mapM rpar tasks)

splitOnDoubleNewline :: String -> [String]
splitOnDoubleNewline s = case breakOn "\n\n" s of
    (chunk, []) -> [chunk]
    (chunk, rest) -> chunk : splitOnDoubleNewline (drop 2 rest)
  where
    breakOn needle haystack = go haystack []
      where
        go [] acc = (reverse acc, [])
        go str@(c : cs) acc
            | take nLen str == needle = (reverse acc, str)
            | otherwise = go cs (c : acc)
        nLen = length needle

parsePlaneBlock :: String -> Maybe (Int, [Point])
parsePlaneBlock block = case lines (trimBlock block) of
    (header : ptLines)
        | not (null header) ->
            let headerWords = words header
             in case headerWords of
                    [nStr, mStr] ->
                        let n = read nStr :: Int
                            m = read mStr :: Int
                            pts = mapMaybe parsePointLine (take m ptLines)
                         in if length pts == m then Just (n, pts) else Nothing
                    _ -> Nothing
    _ -> Nothing
  where
    trimBlock = unlines . filter (not . null) . lines
    parsePointLine ln = case words ln of
        [xStr, yStr] -> Just (read xStr, read yStr)
        _ -> Nothing

parsePlanes :: String -> [(Int, [Point])]
parsePlanes = mapMaybe parsePlaneBlock . splitOnDoubleNewline

benchPlane :: Int -> [Point] -> IO (Double, Double)
benchPlane segs pts = do
    let cands = generateCandidates pts

    (pts `deepseq` cands `deepseq` segs) `seq` return ()

    (sr, sms) <- timeIt $ return $! solve segs cands pts
    (pr, pms) <- timeIt $ return $! solveParallel segs cands pts

    let m = length pts
        rs = maybe "--" (const "OK") sr
        rp = maybe "--" (const "OK") pr
        sp = if pms > 0.001 then sms / pms else 0.0 :: Double

    printf "%-4d %-6s %-6s %10.2f %10.2f %7.2fx\n" m rs rp sms pms sp
    return (sms, pms)

timeIt :: IO a -> IO (a, Double)
timeIt action = do
    t0 <- getCurrentTime
    r <- action
    t1 <- getCurrentTime
    return (r, realToFrac (diffUTCTime t1 t0) * 1000)

main :: IO ()
main = do
    result <- try (readFile "plane.txt") :: IO (Either IOException String)
    content <- case result of
        Left _ -> error "plane.txt missing or unreadable — run generators-exe first to generate it"
        Right s -> return s

    let planes = parsePlanes content
    when (null planes) $ putStrLn "no planes parsed from plane.txt"

    putStrLn (replicate 66 '-')
    printf "%-4s %-6s %-6s %10s %10s %8s\n" "m" "seq" "par" "seq(ms)" "par(ms)" "speedup"
    putStrLn (replicate 66 '-')

    rows <- forM planes $ \(segs, pts) -> benchPlane segs pts

    putStrLn (replicate 66 '-')
    let hard = filter (\(s, _) -> s > 20.0) rows
        avg = if null hard then 0 else sum [s / p | (s, p) <- hard] / fromIntegral (length hard)
    printf "  Total seq:           %8.2f ms\n" (sum $ map fst rows)
    printf "  Total par:           %8.2f ms\n" (sum $ map snd rows)
    printf "  Avg speedup (>20ms): %8.2fx\n" (avg :: Double)
