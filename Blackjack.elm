module Blackjack where

import Random
import Basics (..)
import Text
import Graphics.Element (..)
import Graphics.Input (..)
import Graphics.Collage (..)
import Color (..)
import List (..)
import Signal (Signal, foldp, (<~), (~), constant, sampleOn, keepWhen, subscribe, Channel, channel, send)
import Window 
import Time (Time)
import Time
import Maybe (Maybe(Just, Nothing))

-----------
-- Model --
-----------

faces = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13]
suits = ['H', 'D', 'C', 'S']

type alias Card = (Int,Char)
type alias DeckState = { deck: List Card, hand: List Card, seed: Random.Seed }
type State = Play DeckState
           | End DeckState String
           | Init String

type Ending = Blackjack | Legal | Bust
type Command = Hit | Stay | StartOver

--The channel used by every button to alter the state of the program. The
--channel function requires the initial state of the program, in this case
--StartOver.
updateChan : Channel Command
updateChan = channel StartOver

--Build the deck
allCards faces suits =
  case suits of
    x :: xs -> family faces x ++ allCards faces xs
    _ -> []

family faces suit =
  case faces of
    x :: xs -> 
        (,) x suit :: family xs suit
    _ -> []

initDeck : Random.Seed -> DeckState
initDeck s = { deck = allCards faces suits, hand = [], seed = s}

--Time-based seeding of random; taken from Joseph Collard's TimeBasedDice
start = (\ (t,_) -> t) <~ (Time.timestamp <| constant ())

startTime : Signal (Maybe Time)
startTime = (\ (t,_) -> Just t) <~ sampleOn (Time.fps 10) (Time.timestamp <| constant ())

okayToSample : Int -> Bool
okayToSample fired = fired < 2

okay : Signal Bool
okay = okayToSample <~ (foldp (+) 0 (sampleOn startTime (constant 1)))

seed : Signal (Maybe Random.Seed)
seed = (\ t -> 
            case t of
              Nothing -> Nothing
              Just t -> Just << Random.initialSeed <| round t) <~ keepWhen okay Nothing startTime

------------
-- Update --
------------

--Get the total of the hand, and return it as an Ending union type
handStatus : DeckState -> Ending
handStatus {hand} =
  let cardList = map cardReplace (map fst hand)
      total = replaceOnes cardList (sum cardList)
  in
    if | total == 21 -> Blackjack
       | total < 21  -> Legal
       | otherwise   -> Bust

--View the Ending union type as text
handStatusAsText : Ending -> String
handStatusAsText end =
    case end of
        Blackjack -> "You won!"
        Legal -> "You didn't lose."
        Bust -> "You went bust. :("

--Decide whether to value an ace as 11
replaceOnes : List Int -> Int -> Int
replaceOnes cards total =
    let hasAces = member 1 cards
    in
        case hasAces of
            False -> total
            True -> if ((total + 10) <= 21) then (total + 10) else total

--Replace face card values with 10
cardReplace : Int -> Int
cardReplace face =
  case face of
    11 -> 10
    12 -> 10
    13 -> 10
    x  -> x

--Get from a list based on an index.
getFromList : Int -> List a -> (a, List a)
getFromList index list =
  let prefix = take index list             --take items before the index
      (item :: postfix) = drop index list  --drop the remainder, but keep the item & postfix
  in (item, prefix ++ postfix)             --return the item & remaining list as a tuple

--Pick a card from the deck, and place it in your hand.
pickCard : DeckState -> DeckState
pickCard {deck,hand,seed} =
  let (index, seed') = Random.generate (Random.int 0 (length deck - 1)) seed
      (item, newDeck) = getFromList index deck
      newHand = item :: hand
  in 
      { deck = newDeck, hand = newHand, seed = seed' }

--Call pickCard twice when starting a game
drawTwo deck = pickCard (pickCard deck)

--Main 'step' function for updating the program state
handleInput : (Maybe Random.Seed, Command) -> State -> State
handleInput (maybeSeed, cmd) state = 
  --Each of these 'commands' is triggered only when its button is pressed.
  case cmd of
    StartOver -> --Starts the player over with a fresh hand
      case maybeSeed of
        Just seed ->
          case state of
            --Set with the original seed on Init, or continue with the
            --current seed on Play/End.
            Play d -> Play (drawTwo (initDeck d.seed))
            End d s -> Play (drawTwo (initDeck d.seed))
            Init _ -> Play (drawTwo (initDeck seed))
        Nothing -> state
    Hit -> --Adds a card to the hand, Ends if the hand busts
      case state of
        Play deck -> 
          let d = pickCard deck
              status = handStatus d
          in case status of
            --Forces the End state only if the hand total > 21.
            Bust      -> End d "You went bust!" 
            otherwise -> Play d
        --Do nothing on Hit if the state is End/Init
        otherwise -> state
    Stay -> --Ends the current game
      case state of
        --Return the handStatus in text form if you were in the Play
        --state, or leave a sarcastic reminder if the current game is
        --already over. 
        Play deck -> End deck (handStatusAsText (handStatus deck))
        End d s -> End d "It's over already!"
        Init _ -> state

main : Signal Element
main = view <~ Window.dimensions ~ 
    (foldp handleInput (Init "Loading...") ((,) <~ seed ~ subscribe updateChan))

--Summarizes to Signal.map2 view screenDimensions state, where state is 
--'foldp step startState signalToChangeOn'. In this case, signalToChangeOn 
--is a tuple of the seed and a Signal.channel that updates every time a button
--is pressed.

------------
--  View  --
------------

view : (Int,Int) -> State -> Element
view (w,h) state =
  --let pos = bottomRightAt (absolute 10) (absolute 10)
        container w h middle (
        container (round (toFloat w * 0.8)) h middle (
        flow down [
            buttons,
            displayState state
        ]))

--Helpful pseudo-constant
buttonSize : number
buttonSize = 80

--Return the current state as an element. In the case of Play, return the
--rendered hand; for End, return the hand and the win/lose text.
displayState : State -> Element
displayState state =
    case state of
        Play allDeck -> handRender buttonSize allDeck.hand
        End deck s -> flow down [handRender buttonSize deck.hand, Text.plainText s]
        Init s -> Text.plainText s

--Render the hand as a list of rectangular elements. 'collage' allows
--for the creation of shapes and altering the properties of forms.
handRender : Float -> List Card -> Element
handRender w hand =
    let w' = round w --collage takes Ints; rect takes floats
        cards = displayCardsAsText hand
        drawCard cardtext =
            collage w' (w' * 2) [ group [ 
                rect (w - 4) (w*1.25) |> filled lightGray,
                rect (w - 4) (w*1.25) |> outlined (solid blue),
                toForm (Text.plainText cardtext) ] ]
    in flow right (map drawCard cards)

--Given the hand, return a list of strings where face cards are the
--correct character, and the suits return as HTML escapes.
displayCardsAsText : List Card -> List String
displayCardsAsText cards =
    case cards of
        x :: xs -> ((displayNum (fst x)) ++ (displaySuit (snd x))) :: displayCardsAsText xs
        _ -> []

displayNum : Int -> String
displayNum face =
    case face of
        1  -> "A"
        11 -> "J"
        12 -> "Q"
        13 -> "K"
        x  -> (toString x)

displaySuit : Char -> String
displaySuit suit =
    case suit of
        'C' -> "&clubs;"
        'S' -> "&spades;"
        'H' -> "&hearts;"
        'D' -> "&diams;"

--Create the buttons, and align them in a row
buttons: Element
buttons =
    flow right [
        myButton lightGreen darkCharcoal (buttonSize*2) buttonSize Hit "Hit",
        myButton lightYellow darkCharcoal (buttonSize*2) buttonSize Stay "Stand",
        myButton lightRed yellow (buttonSize*2) buttonSize StartOver "Start Over"
    ]

--customButton has four arguments; the Signal.Channel it sends to,
--and three elements that represent the button in its inactive, hover,
--and click states. 'btn alpha' does most of the work of creating the
--button, and the three button elements are created from it, with different
--alpha values for their background color.
myButton : Color -> Color -> Int -> Int -> Command -> String -> Element
myButton background foreground w h command name =
    let n = min w h
        btn alpha =
            layers [ container w h middle (txt 0.3 foreground name)
                      --|> container (w-1) (h-1) midLeft
                      |> color background
                   , color (rgba 0 0 0 alpha) (spacer w h)
                   ]
    in  customButton (send updateChan command) (btn 0) (btn 0.05) (btn 0.1)
    
txt : Float -> Color -> String -> Element
txt p clr string =
    Text.fromString string
      |> Text.color clr
      |> Text.typeface ["Helvetica Neue","Sans-serif"]
      |> Text.height (p * buttonSize)
      |> Text.leftAligned