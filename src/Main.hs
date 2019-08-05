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
    = ActionInc
    | ActionNone
    | ActionPlay
    deriving (Show, Eq)

main :: IO ()
main = startApp App
    { initialAction = ActionNone
    , model  = Model 0
    , update = updateModel
    , view   = viewModel
    , events = defaultEvents
    , subs   = []
    , mountPoint = Nothing
    }

updateModel :: Action -> Model -> Effect Action Model
updateModel ActionInc m = noEff m { _val = 1 + _val m }
updateModel ActionNone m = noEff m
updateModel ActionPlay m = m <# do
    jsPlay
    pure ActionNone

viewModel :: Model -> View Action
viewModel m = div_ []
    [ h1_ [] [ text "miso-invaders" ]
    , p_ [] [ audio_ [ id_ "myaudio", src_ "47.mp3", controls_ False ] [] ]
    , p_ [] [ text "Touch Bob to play a sound !" ]
    , p_ [] [ img_ [ src_ "spongebob-small.png", onClick ActionPlay ] ]
    , p_ [] 
         [ a_ [ href_ "https://gitlab.com/juliendehos/miso-invaders"] [ text "source code" ]
         , text " / "
         , a_ [ href_ "https://juliendehos.gitlab.io/miso-invaders"] [ text "demo" ]
         ]
    ]

foreign import javascript unsafe "myaudio.play();"
    jsPlay :: IO ()

