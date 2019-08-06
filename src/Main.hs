{-# LANGUAGE OverloadedStrings #-}

import qualified Game as G

import qualified Data.Set as S
import qualified JavaScript.Web.Canvas as JSC

import Data.Map (singleton)
import Miso
import Miso.String hiding (singleton, take)
import System.Random (newStdGen, randoms)

paddleWidth, paddleHeight :: Double
paddleWidth = 70
paddleHeight = 66

paddleImgName :: MisoString
paddleImgName = "spongebob-small.png"

data Model = Model
    { _game :: G.Game
    } deriving (Eq)

data Action
    = ActionNone
    | ActionUpdate -- Double
    | ActionKey (S.Set Int)

main :: IO ()
main = do
    paddleImg <- jsNewImage
    jsSetSrc paddleImg paddleImgName
    -- myRands <- randoms <$> newStdGen
    myRands <- take 1000 . randoms <$> newStdGen
    let game0 = G.createGame myRands paddleWidth paddleHeight
    startApp App 
        { initialAction = ActionNone
        , update        = updateModel paddleImg
        , view          = viewModel
        , model         = Model game0
        , subs          = [ keyboardSub ActionKey ]
        , events        = defaultEvents
        , mountPoint    = Nothing
        }

viewModel :: Model -> View Action
viewModel _ = div_ []
    [ h1_ [] [ text "miso-invaders" ]
    , p_ [] [ audio_ [ id_ "myaudio", src_ "47.mp3" ] [] ]
    , p_ [] [ canvas_ [ id_ "mycanvas" , width_ (ms G.gameWidth), height_ (ms G.gameHeight)
                      , style_  (singleton "border" "1px solid black")
                      ] []
            ]
    , p_ [] 
         [ a_ [ href_ "https://gitlab.com/juliendehos/miso-invaders"] [ text "source code" ]
         , text " / "
         , a_ [ href_ "https://juliendehos.gitlab.io/miso-invaders"] [ text "demo" ]
         ]
    ]

updateModel :: JSC.Image -> Action -> Model -> Effect Action Model
updateModel _ ActionNone m = noEff m
updateModel img ActionUpdate m = m { _game = g' } <# do
    ctx <- jsGetCtx
    JSC.clearRect 0 0 G.gameWidth G.gameHeight ctx
    jsDrawImage img x 100 ctx
    -- jsDrawImage img x y ctx
    pure ActionNone
    where g = _game m
          p = G._paddle g
          (x, y) = G._pos p
          x' = if x > G.gameWidth then 0 else x + 2
          p' = p { G._pos = (x', y) }
          g' = g { G._paddle = p' }
updateModel _ (ActionKey ks) m = 
    m <# (return $ if S.member 37 ks then ActionUpdate else ActionNone)

foreign import javascript unsafe "$r = new Image();"
    jsNewImage :: IO JSC.Image

foreign import javascript unsafe "$1.src = $2;"
    jsSetSrc :: JSC.Image -> MisoString -> IO ()

foreign import javascript unsafe "$r = mycanvas.getContext('2d');"
    jsGetCtx :: IO JSC.Context

foreign import javascript unsafe "$4.drawImage($1, $2, $3);"
    jsDrawImage :: JSC.Image -> Double -> Double -> JSC.Context -> IO ()


{- 

import Control.Monad (when)
import Data.JSString (singleton)
import GHCJS.Types (JSVal)

-- https://github.com/Lermex/miso-plane/blob/master/src/Update.hs

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

updateCanvas :: Model -> IO ()
updateCanvas m = do
    myCtx <- jsGetCtx
    JSC.clearRect 0 0 G.gameWidth G.gameHeight myCtx
    -- let (x, y) = _pos $ _paddle $ _game m
    -- jsDrawImage (_paddleImg m) x y myCtx
    jsDrawImage (_paddleImg m) (_x m) 100 myCtx

foreign import javascript unsafe "myaudio.play();"
    jsPlayAudio :: IO ()

foreign import javascript unsafe "$r = new Date();"
    newDate :: IO JSVal

foreign import javascript unsafe "$r = $1.getMilliseconds() + 1000 * $1.getSeconds();"
    getTime :: JSVal -> IO Double

foreign import javascript unsafe "console.log($1);"
    jsPrint :: MisoString -> IO ()

-}

