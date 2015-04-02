--Slideshow-style code! Unfinished.

import Graphics.Element (..)
import Graphics.Collage (..)
import Signal (Signal, map, foldp, Channel, channel, (<~), (~))
import Keyboard
import String
import Text (asText, style, fromString, centered, Style)
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
      flow down
      [ headerText (w,h) "Hey, mom!"
      , asText "Hey, mom!"
      , asText pres.currentSlide
      ]
    )
  )

headerStyle : Style
headerStyle = 
  { typeface = [ "Helvetica", "sans-serif" ]
  , height   = Just 48
  , color    = black
  , bold     = True
  , italic   = False
  , line     = Nothing
  }

headerText : (Int,Int) -> String -> Element
headerText (w,h) string =
  let viewText str = centered (style headerStyle (fromString str))
      w' = round ((toFloat w) / 1.25)
      h' = h // 9
      nurect = collage w' h'
        [ rect (toFloat w') (toFloat h') |> filled yellow ]
  in
    container w' h' midTop (
      flow outward
        [ nurect
        , viewText string
        ]
    )

positionElement : Element -> Element
positionElement elem =
  container 300 300 midTop elem

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
