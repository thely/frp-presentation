import Mouse
import Window
import Signal ((<~), (~))
import Graphics.Collage (..)
import Graphics.Element (..)
import Color (..)
import Text (asText)

scene (x,y) (w,h) = 
  let (dx, dy) = (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
  in collage w h
    [ ngon 3 100 |> filled (hsla (toFloat x) 0.9 0.6 0.7)
                 |> scale (dy/100)
                 |> rotate (atan2 dy dx)
                 |> move ((dx/5), (dy/5))
    , ngon 6 30  |> filled orange
                 |> move (dx, dy)
    , toForm (asText (x, y)) |> move (0, -30)
    , toForm (asText (w, h)) |> move (0, 0)
    , toForm (asText (dx, dy)) |> move (0, 30)
    ]

main = scene <~ Mouse.position ~ Window.dimensions
