{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import qualified Data.Set as S
import Data.Map (singleton)
import JavaScript.Web.Canvas
import Miso
import Miso.String hiding (singleton)

-- https://github.com/Lermex/miso-plane/blob/master/src/Update.hs
-- https://github.com/dmjio/miso/blob/master/examples/canvas2d/Main.hs

instance Eq Image

data Model = Model
    { _canFire :: Bool
    , _paddleImg :: Image
    , _paddleX :: Double
    , _paddleY :: Double
    , _keys :: S.Set Int
    } deriving (Eq)

data Action
    = ActionNone
    | ActionKey (S.Set Int)
    deriving (Show, Eq)

-- 37 left arrow ( x = -1 )
-- 38 up arrow ( y = 1 )
-- 39 right arrow ( x = 1 )
-- 40 down arrow ( y = -1 )

main :: IO ()
main = do
    bobImg <- jsNewImage
    jsSetSrc bobImg "spongebob-small.png"
    startApp App
        { initialAction = ActionNone
        , model         = Model True bobImg 300 330 S.empty
        , update        = updateModel
        , view          = viewModel
        , events        = defaultEvents
        , subs          = [ keyboardSub ActionKey ]
        , mountPoint    = Nothing
        }

updateModel :: Action -> Model -> Effect Action Model
updateModel ActionNone m = noEff m
updateModel (ActionKey ks) m = m  <# do
    when (S.member 32 ks && _canFire m) jsPlayAudio
    myCtx <- jsGetCtx
    clearRect 0 0 600 400 myCtx
    jsDrawImage (_paddleImg m) x' (_paddleY m) myCtx
    pure ActionNone
    where dx1 = if S.member 37 ks then (-20) else 0
          dx2 = if S.member 39 ks then 20 else 0
          x' = _paddleX m + dx1 + dx2

viewModel :: Model -> View Action
viewModel _ = div_ []
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

