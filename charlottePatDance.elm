import Graphics.Element (..)
import Graphics.Collage (..)
import Basics (..)
import Time (..)
import Signal (..)

main : Signal Element
main = view <~ timeNext

timeNext : Signal Time
timeNext = map (inSeconds << fst) (timestamp (fps 25))

view : Float -> Element
view t = flow outward [ leftRightCharlotte t, upDownPat t ]
--view t = charlottePatDoubleDance t


leftRightCharlotte t = moveImg (wiggle t) 0 charlotte
upDownPat t = moveImg 0 (waggle t) pat
charlottePatDance t = hvDance charlotte pat t
charlottePatDoubleDance t = 
  let cpd = toForm (charlottePatDance t)
  in hvDance cpd cpd t

moveImg x y img = collage 400 400 [ move (x,y) img ]

hvDance : Form -> Form -> Float -> Element
hvDance im1 im2 t =
  flow outward
  [ moveImg (wiggle t) 0 im1
  , moveImg 0 (waggle t) im2
  ]

charlotte : Form
charlotte = toForm (image 50 50 "/stack.jpg")

pat : Form
pat = toForm (image 50 50 "/book.jpg")

wiggle : Float -> Float
wiggle t = 75 * (sin (pi * t))

waggle : Float -> Float
waggle t = 75 * (cos (pi * t))