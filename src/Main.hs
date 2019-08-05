{-# LANGUAGE OverloadedStrings #-}

import Miso
import Miso.String

-- TODO test audio
-- TODO test image
-- TODO test animation
-- TODO implement game

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
    , text (ms $ _val m)
    , button_ [ onClick AddOne ] [ text "+" ]
    , img_ [ src_ "spongebob-small.png" ]
    ]

