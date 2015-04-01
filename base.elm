import Graphics.Element (..)
import Graphics.Collage (..)
import Signal (Signal, map, foldp, Channel, channel, (<~), (~))
import Keyboard
import String
import Text (asText, style, fromString, centered)
import Window
import Color (..)

type alias Presentation = { currentSlide : Int, allSlides : List Slide }
type alias Slide        = 
      { id : Int 
      , slideElements : List SlideElement
      }
type alias SlideElement = { slideType : SlideType, alignment : Alignment, gridSize : Int }

type Alignment = Left | Right | Center | Custom String
type SlideType   = ImageElem String | TextElem String | HeaderElem String
type SlideChange = StartOver | Prev | Next | Stay


updateSlide : Channel SlideChange
updateSlide = channel StartOver

view (w, h) pres = 
  color grey (    
    container w h middle (
      flow down [
        --collage (w//2) (h//2) [ (filled yellow (rect (toFloat w/2) (toFloat h/2))) ],
        headerText (w,h),
        asText "Hey, mom!",
        asText pres.currentSlide
      ]
    )
  )

headerStyle = 
  { typeface = [ "Helvetica", "sans-serif" ]
  , height   = Just 24
  , color    = black
  , bold     = True
  , italic   = False
  }

headerText (w,h) =
  collage (round ((toFloat w)/1.25)) (h//9) [
    (filled yellow (rect ((toFloat w)/1.25) (toFloat h/9))),
    centered (style headerStyle (fromString "Header text yo"))
  ]

basePresent : Presentation
basePresent = { currentSlide = 0, allSlides = [] }


--handleInput : Signal {Int, Int} -> Signal Bool -> SlideChange
handleInput {x} enter =
  if | x     == -1    -> Prev
     | x     ==  1    -> Next
     | enter ==  True -> StartOver
     | otherwise      -> Stay

--update : (Signal {}, Signal Bool) -> Presentation -> Presentation
update keyz pres =
  let choice = handleInput (fst keyz) (snd keyz)
      curr   = pres.currentSlide
  in
    case choice of
      StartOver -> { pres | currentSlide <- 0 }
      Prev      -> { pres | currentSlide <- curr - 1 }
      Next      -> { pres | currentSlide <- curr + 1 }
      Stay      -> pres

state = foldp update basePresent ((,) <~ Keyboard.arrows ~ Keyboard.enter)

main : Signal Element
main = view <~ Window.dimensions ~ state


--main = view <~ Window.dimensions ~ Keyboard.arrows ~ Keyboard.enter
