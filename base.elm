import Graphics.Element (..)
import Signal (Signal, map, foldp)
import Keyboard
import String, Text
import Window

type alias Presentation = { currentSlide : Int, allSlides : List Slide }
type alias Slide        = 
      { id : Int 
      , slideType : SlideType 
      , title : Maybe String
      , text : Maybe String
      , other : Maybe String
      }
type SlideType   = TextOnly | LeftImg | RightImg
type SlideChange = StartOver | Prev | Next

updateSlide : Channel SlideChange
updateSlide = channel StartOver


{-
main : Signal Element
main =
  map asText countClick


countClick : Signal Int
countClick =
  foldp (\clk count -> count + 1) 0 Mouse.clicks
-}
