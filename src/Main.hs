{-# LANGUAGE OverloadedStrings #-}

import Data.Map (singleton)
-- import JavaScript.Web.Canvas
import Miso
import Miso.String hiding (singleton)

-- https://github.com/Lermex/miso-plane/blob/master/src/Update.hs
-- https://github.com/dmjio/miso/blob/master/examples/canvas2d/Main.hs

data Model = Model
    { _val :: Int
    } deriving (Eq)

data Action
    = ActionNone
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
    , p_ [] [ canvas_ [ id_ "mycanvas" , width_ "600" , height_ "400"
                      , style_  (singleton "border" "1px solid black")
                      , onMouseDown ActionPlay ] [] ]

    , p_ [] 
         [ a_ [ href_ "https://gitlab.com/juliendehos/miso-invaders"] [ text "source code" ]
         , text " / "
         , a_ [ href_ "https://juliendehos.gitlab.io/miso-invaders"] [ text "demo" ]
         ]
    ]

foreign import javascript unsafe "myaudio.play();"
    jsPlay :: IO ()

