-- Author: Benjamin Schulz
-- License: http://creativecommons.org/licenses/by-nc-sa/4.0/

import Util ((<|), (|>))
import Data.List

nElem = 16

joinWords [] = ""
joinWords xs = intercalate " " xs

joinLines [] = ""
joinLines xs = intercalate "\n" xs

preNumSymbols = "SY drvOffset=5.0"

indexedSymbol symbolName valueStrings =
    zipWith (\i v->(show i, v)) [1..] valueStrings |>
    map (\(i,v)->"SY " ++ symbolName ++ i ++ "=" ++ v) |> joinLines

lengths = indexedSymbol "l" <| map show <| take nElem
          [60.46, 59.40, 56.03, 54.43, 53.19,
           52.48, 51.77, 51.42, 51.06, 50.71,
           50.36, 50.00, 49.65, 49.47, 49.29,
           49.11, 48.94, 48.76, 48.58, 48.41,
           48.23, 48.05, 47.87, 47.87, 47.70,
           47.70]

positions = indexedSymbol "p" <| map show <| take nElem
            [0.0, 18.44, 25.89, 39.72, 58.87,
             82.63, 110.29, 141.49, 175.54, 212.06,
             250.71, 291.14, 333.16, 376.25, 420.75,
             466.14, 512.42, 559.23, 606.75, 654.80,
             703.38, 752.14, 801.43, 850.72, 900.55,
             950.37]

radii = indexedSymbol "r" <| take nElem (repeat "awg14")



lengthPositionWireCard n =
    let ns = show n
        segments = "9" in
    joinWords [
     "GW "++ns, segments,
     "p"++ns, "0", "-l"++ns++"/2",
     "p"++ns, "0", "l"++ns++"/2",
     "r"++ns]

driverCard n =
    let n0 = show n
        n1 = show (n+101)
        n2 = show (n+102)
        n3 = show (n+103)
        p0 = "p"++n0++"-"++"drvOffset/2"
        p1 = "p"++n0++"+"++"drvOffset/2"
        y1 = "0" --"drvOffset/2"
        y0 = "0" --"-"++y1
        l1 = "l"++n0++"/2"
        l0 = "-"++l1
        r  = "r"++n0
        s  = "9"
        sc = "1"
    in
    [["GW", n0, s,   p0, y0, l0,   p0, y0, l1,   r],
     ["GW", n1, sc,  p0, y0, l0,   p1, y1, l0,   r],
     ["GW", n2, sc,  p0, y0, l1,   p1, y1, l1,   r],
     ["GW", n3, s,   p1, y1, l0,   p1, y1, l1,   r]]
    |> map joinWords
    |> joinLines
          
wires = lengthPositionWireCard 1
        : (driverCard 2)
        : (map lengthPositionWireCard [3..nElem])
        |> joinLines

numSymbols = [preNumSymbols,
              positions,
              lengths] |> joinLines

symbolicSymbols = ["SY awg14=1.7/2", radii] |> joinLines

post = ["GS 0 0 0.001",
        "GE 0",
        "GN -1",
        "EK",
        "EX 0 2 5 0 1 0",
        "FR 0 0 0 0 2437 0",
        "EN"] |> joinLines

main = do
  putStrLn <| joinLines [numSymbols,
                         symbolicSymbols,
                         wires,
                         post]
