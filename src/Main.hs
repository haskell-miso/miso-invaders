{-# LANGUAGE OverloadedStrings #-}

import Data.Map (singleton)
import JavaScript.Web.Canvas
import Miso
import Miso.String hiding (singleton)

-- https://github.com/Lermex/miso-plane/blob/master/src/Update.hs
-- https://github.com/dmjio/miso/blob/master/examples/canvas2d/Main.hs

{-
hKeys :: KeyCode -> Action
hKeys (KeyCode 37) = ActionPlay
hKeys _ = ActionNone
-- 37 left arrow ( x = -1 ) 38 up arrow ( y = 1 ) 39 right arrow ( x = 1 ) 40 down arrow ( y = -1 )
-}

-- https://haddocks.haskell-miso.org/Miso-Subscription-Keyboard.html

instance Eq Image

data Model = Model
    { _val :: Int
    , _paddle :: Image
    } deriving (Eq)

data Action
    = ActionNone
    | ActionPlay
    deriving (Show, Eq)

main :: IO ()
main = do
    bobImg <- jsNewImage
    jsSetSrc bobImg "spongebob-small.png"
    startApp App
        { initialAction = ActionNone
        , model         = Model 0 bobImg
        , update        = updateModel
        , view          = viewModel
        , events        = defaultEvents
        , subs          = []
        , mountPoint    = Nothing
        }

updateModel :: Action -> Model -> Effect Action Model
updateModel ActionNone m = noEff m
updateModel ActionPlay m = m <# do
    jsPlayAudio
    myCtx <- jsGetCtx
    clearRect 0 0 600 400 myCtx
    jsDrawImage (_paddle m) 200 100 myCtx
    pure ActionNone

viewModel :: Model -> View Action
viewModel m = div_ []
    [ h1_ [] [ text "miso-invaders" ]
    , p_ [] [ audio_ [ id_ "myaudio", src_ "47.mp3" ] [] ]
    , p_ [] [ canvas_ [ id_ "mycanvas" , width_ "600" , height_ "400"
                      , style_  (singleton "border" "1px solid black")
                      ] []
            ]
    , p_ [] 
         [ a_ [ href_ "https://gitlab.com/juliendehos/miso-invaders"] [ text "source code" ]
         , text " / "
         , a_ [ href_ "https://juliendehos.gitlab.io/miso-invaders"] [ text "demo" ]
         ]
    ]

foreign import javascript unsafe "myaudio.play();"
    jsPlayAudio :: IO ()

foreign import javascript unsafe "$r = mycanvas.getContext('2d');"
    jsGetCtx :: IO Context

foreign import javascript unsafe "$r = new Image();"
    jsNewImage :: IO Image

foreign import javascript unsafe "$1.src = $2;"
    jsSetSrc :: Image -> MisoString -> IO ()

foreign import javascript unsafe "$4.drawImage($1, $2, $3);"
    jsDrawImage :: Image -> Double -> Double -> Context -> IO ()

