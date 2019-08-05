{-# LANGUAGE OverloadedStrings #-}

import Miso
import Miso.String

-- TODO test animation
-- TODO implement game

-- https://github.com/Lermex/miso-plane/blob/master/src/Update.hs
-- http://hackage.haskell.org/package/miso-1.2.0.0/docs/Miso-Html-Element.html
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio

data Model = Model
    { _val :: Int
    } deriving (Eq)

data Action
    = AddOne
    | NoOp
    deriving (Show, Eq)

main :: IO ()
main = startApp App
    { initialAction = NoOp
    , model  = Model 0
    , update = updateModel
    , view   = viewModel
    , events = defaultEvents
    , subs   = []
    , mountPoint = Nothing
    }

updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff m { _val = 1 + _val m }
updateModel NoOp m = noEff m

viewModel :: Model -> View Action
viewModel m = div_ []
    [ h1_ [] [ text "miso-invaders" ]
    , p_ [] [ text (ms $ _val m), button_ [ onClick AddOne ] [ text "+" ] ]
    , p_ [] [ img_ [ src_ "spongebob-small.png" ] ]
    , p_ [] [ audio_ [ src_ "47.mp3", controls_ True ] [] ]
    ]

