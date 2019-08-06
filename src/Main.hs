{-# LANGUAGE OverloadedStrings #-}

import Game

import qualified Data.Set as S
import qualified JavaScript.Web.Canvas as JSC

import Control.Monad (when)
import Data.Map (singleton)
import GHCJS.Types (JSVal)
import System.Random (newStdGen, randoms)

import Miso
import Miso.String hiding (singleton)

-- https://github.com/Lermex/miso-plane/blob/master/src/Update.hs

instance Eq JSC.Image

data Model = Model
    { _game :: Game
    , _paddleImg :: JSC.Image
    , _x :: Double
    } deriving (Eq)

data Action
    = ActionStopped MisoString
    | ActionGetTime
    | ActionSetTime Double
    | ActionKey (S.Set Int)
    deriving (Show, Eq)

main :: IO ()
main = do
    bobImg <- jsNewImage
    jsSetSrc bobImg "spongebob-small.png"
    myRands <- randoms <$> newStdGen
    let game0 = createGame (myRands :: [Double])
    startApp App
        { initialAction = ActionStopped "welcome"
        , model         = Model game0 bobImg 100
        , update        = updateModel
        , view          = viewModel
        , events        = defaultEvents
        , subs          = [ keyboardSub ActionKey ]
        , mountPoint    = Nothing
        }

updateModel :: Action -> Model -> Effect Action Model
updateModel (ActionStopped msg) m = noEff m
updateModel ActionGetTime m = m <# do
    date <- newDate
    time <- getTime date
    pure $ ActionSetTime time
    -- pure $ ActionStopped ""
updateModel (ActionSetTime time) m = m <# do
    -- jsPrint $ ms time
    -- updateCanvas m
    pure $ ActionStopped ""
    -- pure ActionGetTime

-- updateModel (ActionKey ks) m = m { _x = 5 + _x m } <# do
updateModel (ActionKey ks) m = m <# do
    jsPrint $ ms $ show ks
    -- updateCanvas m
    -- pure $ ActionStopped ""
    pure ActionGetTime

{-
updateModel (ActionKey ks) m = m  <# do
    when (S.member 32 ks && _canFire m) jsPlayAudio
    myCtx <- jsGetCtx
    clearRect 0 0 600 400 myCtx
    jsDrawImage (_paddleImg m) x' (_paddleY m) myCtx
    pure ActionNone
    where dx1 = if S.member 37 ks then (-20) else 0
          dx2 = if S.member 39 ks then 20 else 0
          x' = _paddleX m + dx1 + dx2
-}

viewModel :: Model -> View Action
viewModel _ = div_ []
    [ h1_ [] [ text "miso-invaders" ]
    , p_ [] [ audio_ [ id_ "myaudio", src_ "47.mp3" ] [] ]
    , p_ [] [ canvas_ [ id_ "mycanvas" , width_ (ms gameWidth), height_ (ms gameHeight)
                      , style_  (singleton "border" "1px solid black")
                      ] []
            ]
    , p_ [] 
         [ a_ [ href_ "https://gitlab.com/juliendehos/miso-invaders"] [ text "source code" ]
         , text " / "
         , a_ [ href_ "https://juliendehos.gitlab.io/miso-invaders"] [ text "demo" ]
         ]
    ]

updateCanvas :: Model -> IO ()
updateCanvas m = do
    myCtx <- jsGetCtx
    JSC.clearRect 0 0 gameWidth gameHeight myCtx
    -- let (x, y) = _pos $ _paddle $ _game m
    -- jsDrawImage (_paddleImg m) x y myCtx
    jsDrawImage (_paddleImg m) (_x m) 100 myCtx

foreign import javascript unsafe "myaudio.play();"
    jsPlayAudio :: IO ()

foreign import javascript unsafe "$r = mycanvas.getContext('2d');"
    jsGetCtx :: IO JSC.Context

foreign import javascript unsafe "$r = new Image();"
    jsNewImage :: IO JSC.Image

foreign import javascript unsafe "$1.src = $2;"
    jsSetSrc :: JSC.Image -> MisoString -> IO ()

foreign import javascript unsafe "$4.drawImage($1, $2, $3);"
    jsDrawImage :: JSC.Image -> Double -> Double -> JSC.Context -> IO ()

foreign import javascript unsafe "$r = new Date();"
    newDate :: IO JSVal

foreign import javascript unsafe "$r = $1.getMilliseconds() + 1000 * $1.getSeconds();"
    getTime :: JSVal -> IO Double

foreign import javascript unsafe "console.log($1);"
    jsPrint :: MisoString -> IO ()

