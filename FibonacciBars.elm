import Basics (..)
import List (..)
import Color (..)
import Graphics.Element (..)
import Graphics.Collage (..)
import Text (asText)

--Calculate
fibonacci : Int -> Int
fibonacci n =
  case n of
    0 -> 0
    1 -> 1
    _ -> fibonacci (n-1) + fibonacci (n-2)

fibonacciWithIndexes : Int -> List (Int,Int)
fibonacciWithIndexes n = map2 (,) [0..n] (map fibonacci [0..n])

--View
color: Int -> Color
color n =
  let colors = [red, orange, yellow, green, blue, purple, brown]
  in head (drop (n % (length colors)) colors)

bar: (Int, Int) -> Element
bar (index, n) = 
    flow right 
    [ collage (n*20) 20 
        [ (rect (toFloat n * 20) 20) |> filled (color index) ]
    , asText n 
    ]

main = flow down <| map bar (fibonacciWithIndexes 10)
