import qualified Data.Vector.Unboxed as V
import Control.Applicative
import Data.List (intercalate)
import Data.List.Split (chunksOf)

import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine
import System.Environment (withArgs)

import Codec.Picture --(PixelRGB8, Image, generateImage)

-- Part 1

input = parse <$> readFile "day18Input.txt"
part1 = V.length . V.filter (==True) . (!!100 ) . iterate nextStep <$> input >>= print

parse :: String -> V.Vector Bool
parse = V.fromList . map f . filter (`elem` "#.")
  where f '#' = True
        f '.' = False

unparse :: V.Vector Bool -> [String]
unparse vec = chunksOf (isqrt . V.length $ vec) . map f . V.toList $ vec
  where f True = '#'
        f False = '.'

nextStep :: V.Vector Bool -> V.Vector Bool
nextStep lights = V.imap (updateLight lights) lights

updateLight :: V.Vector Bool -> Int -> Bool -> Bool
updateLight lights i l = (l && (c == 2 || c == 3)) || (not l && c == 3)
  where c = countNeighbours lights i

countNeighbours :: V.Vector Bool -> Int -> Int
countNeighbours lights i = length . filter ((==True) . (V.!) lights) $ neighbours (isqrt . V.length $ lights) i

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

neighbours :: Int -> Int -> [Int]
neighbours width i = [xn+yn*width |xn <- [x-1..x+1], yn <-[y-1..y+1], clip xn, clip yn, xn+yn*width /= i]
  where x = i `rem` width
        y = (i - x) `quot` width
        clip a = a >= 0 && a < width

-- Part 2
updateLight2 :: V.Vector Bool -> Int -> Bool -> Bool
updateLight2 lights i l
  | i `elem` [0,width-1,(width-1)*width, width*width-1] = True
  | l && (c == 2 || c == 3) = True
  | not l && c == 3         = True
  | otherwise               = False
    where c = countNeighbours lights i
          width = isqrt . V.length $ lights

nextStep2 :: V.Vector Bool -> V.Vector Bool
nextStep2 lights = V.imap (updateLight2 lights) lights

part2 = V.length . V.filter (==True) . (!!100 ) . iterate nextStep2 <$> input >>= print

-- Animate
lightDiagram :: [Bool] -> Diagram B
lightDiagram vec = vcat . map hcat . chunksOf (isqrt . length $ vec) . map f $ vec
  where f :: Bool -> Diagram B
        f True = square 1 # lw none # fc black
        f False = square 1 # lw none # fc white

dias :: [[Bool]] -> [Diagram B]
dias = map lightDiagram

times :: Int -> [Int]
times n = replicate (n-1) 10 ++ [300]

gifs :: [[Bool]] -> [(Diagram B, Int)]
gifs bs = zip (dias bs) (times . length $ bs)

testInput = parse <$> readFile "test.txt"
part3 = gifs . map V.toList . take 10 . iterate nextStep <$> input >>= withArgs ["-o test.gif","-w 300"] . mainWith

-- Animate attempt two (with Juicy pixels)
gridPrint :: V.Vector Bool -> Int -> Int -> Int -> PixelRGB8
gridPrint v width x y = if on then PixelRGB8 128 255 0 else PixelRGB8 0 64 0
  where on = v V.! (x+y*width)

visualize :: V.Vector Bool -> Image PixelRGB8
visualize v = generateImage (gridPrint v width) width width
  where width = isqrt $ V.length v

visualizePart1 = map visualize . take 101 . iterate nextStep <$> input >>= toGifAnimation "day18part1.gif"
visualizePart2 = map visualize . take 101 . iterate nextStep2 <$> input >>= toGifAnimation "day18part2.gif"

toGifAnimation :: FilePath -> [Image PixelRGB8] -> IO ()
toGifAnimation path images =
    case writeGifAnimation path 10 LoopingForever images of
      Left err -> putStrLn err
      Right w -> w


