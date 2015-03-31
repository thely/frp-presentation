import Graphics.Element (..)
import Graphics.Collage (..)
import Signal (Signal, map, foldp, Channel, channel, (<~), (~))
import Keyboard
import String
import Text (asText)
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

view (w, h) keyboard = 
  container w h middle (
    flow down [
      asText "Hey, mom!",
      asText keyboard
    ]
  )

main : Signal Element
main = view <~ Window.dimensions ~ Keyboard.arrows
