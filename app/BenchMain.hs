import Control.Exception (evaluate)
import Control.Monad (forM_, guard)
import Criterion.Main (bench, bgroup, defaultMain, nf)
import Data.BitBoard
import Data.Bits (Bits ((.&.), (.|.)), complement, xor)
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl', groupBy, iterate', sort)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Test.QuickCheck (chooseInt, elements, generate, shuffle, vectorOf)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type Index = (Int, Int)

type Level = Int

toIndex :: Int -> Index
toIndex i = (i `div` 5 + 1, i `mod` 5 + 1)

fromIndex :: Index -> Int
fromIndex (x, y) = (x - 1) * 5 + (y -1)

--------------------------------------------------------------------------------
-- Instance Generator
--------------------------------------------------------------------------------

generateRandomInput :: Int -> IO [(Index, [Level])]
generateRandomInput n = generate $
  vectorOf n $ do
    indices <- shuffle [0 .. 24]
    workerLevel <- chooseInt (0, 2)
    emptyLevels <- vectorOf 21 $ elements [0, 0, 1, 1, 1, 2, 2, 2, 3, 4] -- levels at empty spaces (choose with some biases)
    let spaces = (map snd . sort . zip indices) (workerLevel : [4, 4, 4] ++ emptyLevels)
    let worker = toIndex $head indices
    return (worker, spaces)

--------------------------------------------------------------------------------
-- Computing Move-to
--------------------------------------------------------------------------------

-- Naive
getMoveToNaive :: (Index, [Level]) -> [Index]
getMoveToNaive ((x, y), spaces) =
  let levels = IntMap.fromList $ zip [0 ..] spaces
   in [ (xx, yy)
        | dx <- [-1, 0, 1],
          dy <- [-1, 0, 1],
          dx /= 0 || dy /= 0,
          let xx = x + dx, -- move-to candidate
          let yy = y + dy,
          1 <= xx && xx <= 5, -- boundary check
          1 <= yy && yy <= 5,
          levels ! fromIndex (xx, yy) <= 3, -- cannot move up to level 4
          levels ! fromIndex (x, y) + 1 >= levels ! fromIndex (xx, yy) -- can move up at most one level
      ]

-- With BB
convertInput :: (Index, [Level]) -> (Int, [BitBoard])
convertInput (moveFrom, spaces) =
  ( toBBIndex moveFrom,
    foldl'
      ( \[x0, x1, x2, x3] (i, y) ->
          let bb = singletonBB i
           in case y of
                0 -> [x0 + bb, x1, x2, x3]
                1 -> [x0, x1 + bb, x2, x3]
                2 -> [x0, x1, x2 + bb, x3]
                3 -> [x0, x1, x2, x3 + bb]
                _ -> [x0, x1, x2, x3]
      )
      [0, 0, 0, 0]
      (zip validIndices spaces)
  )

convertOutput :: BitBoard -> [Index]
convertOutput = map fromBBIndex . bbToList

getMoveToWithBB :: (Index, [Level]) -> [Index]
getMoveToWithBB = convertOutput . getMoveToWithBB' . convertInput

getMoveToWithBB' :: (BBIndex, [BitBoard]) -> BitBoard
getMoveToWithBB' (i, [x0, x1, x2, x3]) = case getNeighborhood i of
  nbr | i `elemBB` x0 -> nbr .&. (x0 .|. x1)
  nbr | i `elemBB` x1 -> nbr .&. (x0 .|. x1 .|. x2)
  nbr -> nbr .&. (x0 .|. x1 .|. x2 .|. x3)
getMoveToWithBB' _ = undefined

--------------------------------------------------------------------------------
-- Distance Computation
--------------------------------------------------------------------------------

-- infinity distance
distInf :: Int
distInf = 100

-- Naive BFS
getDistancesNaive :: (Index, [Level]) -> IntMap Int
getDistancesNaive (moveFrom, spaces) =
  let initMap = IntMap.fromList [(i, if i == fromIndex moveFrom then 0 else distInf) | i <- [0 .. 24]]
   in getDistancesNaive' spaces (Seq.fromList [moveFrom]) initMap

getDistancesNaive' :: [Level] -> Seq Index -> IntMap Int -> IntMap Int
getDistancesNaive' spaces q sofar | Seq.null q = sofar
getDistancesNaive' spaces q sofar =
  let x = q `Seq.index` 0
      x' = fromIndex x
      nbrs = getMoveToNaive (x, spaces)
      unseen = Seq.fromList [nbr | nbr <- nbrs, (sofar ! fromIndex nbr) == distInf]
      d = (sofar ! x') + 1
      q' = Seq.drop 1 q Seq.>< unseen
      sofar' = foldl (\m u -> IntMap.insert (fromIndex u) d m) sofar unseen
   in getDistancesNaive' spaces q' sofar'

-- Bitboard-based BFS
getMoveToBB :: [BitBoard] -> [BitBoard] -> [BitBoard]
getMoveToBB [v0, v1, v2, v3] [x0, x1, x2, x3] =
  let xx = x0 .|. x1 .|. x2 .|. x3
      y0 = getClosedNeighborhood xx .&. v0
      y1 = getClosedNeighborhood xx .&. v1
      y2 = getClosedNeighborhood (xx `xor` x0) .&. v2
      y3 = getClosedNeighborhood (x2 .|. x3) .&. v3
   in [y0, y1, y2, y3]
getMoveToBB _ _ = undefined

getDistancesWithBB :: (Index, [Level]) -> IntMap Int
getDistancesWithBB i =
  let (moveFrom, levels) = convertInput i
      result = getDistancesWithBB' (singletonBB moveFrom) levels
      result' = [((fromIndex . fromBBIndex) j, d) | (d, x) <- zip [0 ..] result, j <- bbToList x]
   in IntMap.fromList $ zip [0 .. 24] (repeat distInf) ++ result'

getDistancesWithBB' :: BitBoard -> [BitBoard] -> [BitBoard]
getDistancesWithBB' moveFrom levels = (takeWhile (/= 0) . map (sum . fst)) $ iterate' f (map (.&. moveFrom) levels, moveFrom)
  where
    f (frontier, visited) =
      let ys = getMoveToBB levels frontier
       in (map (`andNotBB` visited) ys, visited .|. sum ys)

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

verify :: (Index, [Level]) -> Bool
verify x = getMoveToNaive x == getMoveToWithBB x && getDistancesNaive x == getDistancesWithBB x

--------------------------------------------------------------------------------
-- Entry Point
--------------------------------------------------------------------------------

main :: IO ()
main = do
  instances <- generateRandomInput 100000
  let instances' = map convertInput instances

  -- check correctness
  guard $ all verify instances

  -- benchmark
  defaultMain
    [ bgroup
        "getMoveTo"
        [ bench "Naive" $ nf (map getMoveToNaive) instances,
          bench "BB" $ nf (map getMoveToWithBB) instances,
          bench "BB (core)" $ nf (map getMoveToWithBB') instances'
        ],
      bgroup
        "getDistances"
        [ bench "Naive" $ nf (map getDistancesNaive) (take 10000 instances),
          bench "BB" $ nf (map getDistancesWithBB) (take 10000 instances)
        ]
    ]
