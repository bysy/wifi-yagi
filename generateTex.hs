-- Author: Benjamin Schulz
-- License: http://creativecommons.org/licenses/by-nc-sa/4.0/

import Util (fmod, foldlc', realNumberP, (|>))
import Data.List
import Text.ParserCombinators.Parsec hiding (getPosition, setPosition)


data Element = Reflector Double                     -- ^ Length
             | FoldedDriver Double Double Double    -- ^ Length Position Spacing
             | Director Double Double               -- ^ Length Position
               deriving (Show)

getPosition (Reflector _) = 0.0
getPosition (FoldedDriver _ p _) = p
getPosition (Director _ p) = p

setPosition (Reflector x) _ = Reflector x
setPosition (FoldedDriver l _ s) p1 = FoldedDriver l p1 s
setPosition (Director l _) p1 = Director l p1



-- All measurements are in mm.
height = 235.0
width = 100.0
wireDiameter = 1.02362
effHeight = height -- - wireDiameter  
-- ^ Used to partition elements that don't fit in a single schematic
-- and, accordingly, as the length of the vertical line.
-- TODO Check mechanism

centerP = width / 2.0
align l = if l <= width then (width - l) / 2.0 else centerP
leftEdge = align



putString :: (Double, Double) -> String -> String
putString pos str = "\\put"++(show pos)++"{"++str++"}"

line :: (Double, Double) -> (Int, Int) -> Double -> String
line pos orient l = putString pos ("\\line"++(show orient)++"{"++(show l)++"}")

horizontalLine, verticalLine :: (Double, Double) -> Double -> String
horizontalLine pos l = line pos (1,0) l
verticalLine   pos l = line pos (0,1) l

toString (Reflector l) i = (putString (0.0, 0.0) ((show i)++": Reflector"))++"\n"++
                           (horizontalLine (leftEdge l, 0.0) l)
                         
toString (FoldedDriver l p s) i =
    let halfS = s / 2.0
        (p0, p1) = (p - halfS, p + halfS)
        le = leftEdge l in
    (putString (0.0, p) ((show i)++": Driver"))++"\n"++
    (horizontalLine (le, p0) l)++"\n"++
    (horizontalLine (le, p1) l)

toString (Director l p) i = (putString (0.0, p) ((show i)++":")) ++ 
                            horizontalLine (leftEdge l, p) l

accumulateLatex :: Integer -> (String, Double) -> Element -> (String, Double)
accumulateLatex i (str, sumLengths) el = case el of
            r@(Reflector l)        ->  (str++(toString r i)++"\n", sumLengths+l)
            d@(FoldedDriver l _ s) ->  (str++(toString d i)++"\n", sumLengths+ 2.0*l + 2.0*s)
            d@(Director l _)       ->  (str++(toString d i)++"\n", sumLengths+l)

generatePic :: [Element] -> String
generatePic es = pre ++ elementDesc ++ post
    where (elementDesc, _) = foldlc' accumulateLatex 1 ("", 0.0) es
          pre = "\\begin{picture}"++(show (width, height))++"\n"++
                "\\linethickness{0.2mm}\n"++
                (verticalLine (centerP, 0.0) effHeight)++"\n"++
                "\\linethickness{"++(show wireDiameter)++"mm}\n"
          post = "\\end{picture}"

generateLatex :: [[Element]] -> String
generateLatex ess = map generatePic ess |> intercalate "\n\n"



main = putStrLn $ generateLatex partitionedElements



-- Generate `partitionedElements' from raw data.

-- Old data for AWG14 wire:
--txtData = "R  59.92656\nV 55.3829   17.95238 5.0\nDi 56.98917  25.97471\nDi 53.32981  39.65718\nDi 53.46347  59.0421\nDi 52.20775  82.48168\nDi 51.93276 110.4369\nDi 50.72735 141.3339\nDi 51.36706 175.6009\nDi 50.60978 211.9554\nDi 49.39323 250.4825\nDi 49.70468 291.0987\nDi 50.19537 333.0188\nDi 49.55065 376.6792\nDi 48.3601  420.7017\nDi 48.45944 463.7104\n"

-- Reoptimized for AWG18 wire:
txtData = "R           59.39285\nV 17.67539   55.18411  5.000235\nDi 26.17821  57.41367\nDi 39.249    54.26\nDi 59.73754  52.34315\nDi 81.2772   52.70218\nDi 110.7897  51.34419\nDi 141.2818  51.22216\nDi 175.0379  51.29109\nDi 209.1281  50.27743\nDi 251.4355  49.71091\nDi 288.9151  49.69382\nDi 330.2744  49.77231\nDi 372.2857  49.76373\nDi 420.8858  48.44581\nDi 461.291   48.16839\n"

dataParser = sepEndBy txtLine (char '\n')
txtLine = 
    do { char 'R'; someSpace; l <- realNumberP; return (Reflector (read l)) } <|>
    do { char 'V'; someSpace; 
         p <- realNumberP; someSpace; l <- realNumberP; someSpace; s <- realNumberP; 
         return (FoldedDriver (read l) (read p) (read s)) } <|>
    do { string "Di"; someSpace; p <- realNumberP; someSpace; l <- realNumberP;
         return (Director (read l) (read p))}
    <?> "element"
someSpace = many1 space

defaultOrRight :: b -> (Either a b) -> b
defaultOrRight def lr = either (\_ -> def) id lr

elements = defaultOrRight [] (parse dataParser "" txtData)

partitionByHeight _ [] = []
partitionByHeight h xs = 
    let hs = [h, 2.0*h..]
        recurse [] _ = []
        recurse xs (h0 : hs) = 
            let (part, rest) = span (\x -> getPosition x <= h0) xs in
            part : (recurse rest hs) in
    recurse xs hs

-- After partitioning, take mod height.
adjustHeights h el = let p0 = getPosition el in
                     setPosition el (fmod p0 h)

-- A list of lists of elements where each list of elements fits into one picture.
partitionedElements = partitionByHeight effHeight elements |>
                      map (map (adjustHeights effHeight))

