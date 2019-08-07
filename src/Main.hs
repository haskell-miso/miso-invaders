{-# LANGUAGE OverloadedStrings #-}

-- TODO audio

import qualified Game as G

import qualified Data.Set as S
import qualified JavaScript.Web.Canvas as JSC
import qualified Miso.String as MS

import Data.Map (singleton)
import Miso
import System.Random (newStdGen, randoms)

----------------------------------------------------------------------
-- types & params
----------------------------------------------------------------------

paddleWidth, paddleHeight :: Double
paddleWidth = 70
paddleHeight = 66

paddleImgName :: MS.MisoString
paddleImgName = "spongebob-small.png"

data Model = Model
    { _game :: G.Game
    , _time :: Double
    , _rands :: [Double]
    } deriving (Eq)

data Action
    = ActionNone
    | ActionReset
    | ActionDisplay
    | ActionStep Double
    | ActionKey (S.Set Int)

----------------------------------------------------------------------
-- main functions
----------------------------------------------------------------------

myGetTime :: IO Double
myGetTime = (* 0.001) <$> now

main :: IO ()
main = do
    paddleImg <- jsNewImage
    jsSetSrc paddleImg paddleImgName
    rands <- take 1000 . randoms <$> newStdGen
    time0 <- myGetTime
    let game0 = G.createGame rands paddleWidth paddleHeight
    startApp App
        { initialAction = ActionNone
        , update        = updateModel paddleImg
        , view          = viewModel
        , model         = Model game0 time0 rands
        , subs          = [ keyboardSub ActionKey ]
        , events        = defaultEvents
        , mountPoint    = Nothing
        }

----------------------------------------------------------------------
-- view function
----------------------------------------------------------------------

viewModel :: Model -> View Action
viewModel _ = div_ []
    [ h1_ [] [ text "miso-invaders" ]
    , p_ [] [ audio_ [ id_ "myaudio", src_ "47.mp3" ] [] ]
    , p_ [] [ "Press enter to start..." ]
    , p_ [] [ canvas_ [ id_ "mycanvas"
                      , width_ (MS.ms G.gameWidth)
                      , height_ (MS.ms G.gameHeight)
                      , style_  (singleton "border" "1px solid black")
                      ] []
            ]
    , p_ []
         [ a_ [ href_ "https://gitlab.com/juliendehos/miso-invaders"]
              [ text "source code" ]
         , text " / "
         , a_ [ href_ "https://juliendehos.gitlab.io/miso-invaders"]
              [ text "demo" ]
         ]
    ]

drawItem :: JSC.Context -> G.Item -> IO ()
drawItem ctx (G.Item (sx, sy) (px, py) _) = 
    JSC.fillRect (px-0.5*sx) (py-0.5*sy) sx sy ctx

drawGame :: JSC.Image -> G.Game -> IO ()
drawGame paddleImg game = do
    -- initialize drawing
    ctx <- jsGetCtx
    JSC.clearRect 0 0 G.gameWidthD G.gameHeightD ctx
    JSC.lineWidth 0 ctx
    -- draw bullets
    JSC.fillStyle 0 0 255 255 ctx
    mapM_ (drawItem ctx) (G._bullets game)
    -- draw invaders
    JSC.fillStyle 255 0 0 255 ctx
    mapM_ (drawItem ctx) (G._invaders game)
    JSC.stroke ctx
    -- draw paddle
    jsDrawImage paddleImg (x-0.5*paddleWidth) (y-0.5*paddleHeight) ctx
    where paddle = G._paddle game
          (x, y) = G._pos paddle

drawText :: MS.JSString -> IO ()
drawText txt = do
    ctx <- jsGetCtx
    JSC.clearRect 0 0 G.gameWidthD G.gameHeightD ctx
    JSC.fillStyle 0 0 0 255 ctx
    JSC.textAlign JSC.Center ctx
    JSC.font "40px Arial" ctx
    JSC.fillText txt (0.5*G.gameWidthD) (0.5*G.gameHeightD) ctx
    JSC.stroke ctx

----------------------------------------------------------------------
-- update function
----------------------------------------------------------------------

updateModel :: JSC.Image -> Action -> Model -> Effect Action Model

updateModel _ ActionNone m = noEff m

updateModel _ ActionReset m = m' <# pure ActionDisplay
    where rands' = G.doCycle 1 (_rands m)
          game' = G.createGame rands' paddleWidth paddleHeight
          m' = m { _game = game', _rands = rands' }

updateModel _ ActionDisplay m = m <#
    case G._status (_game m) of
        G.Won -> drawText "You win !" >> pure ActionNone
        G.Lost -> drawText "Game over !" >> pure ActionNone
        _ -> ActionStep <$> myGetTime

updateModel paddleImg (ActionStep t1) m = m' <# do
    drawGame paddleImg game
    pure ActionDisplay
    where t0 = _time m
          dt = t1 - t0
          game = _game m
          game' = G.step dt game
          m' = m { _game = game', _time = t1 }

updateModel _ (ActionKey ks) m = 
    if S.member 13 ks 
    then m <# (jsConsoleLog "new game" >> pure ActionReset)
    else noEff m { _game = (_game m) { G._inputLeft = S.member 37 ks
                                     , G._inputRight = S.member 39 ks
                                     , G._inputFire = S.member 32 ks
                                     }
                 }

----------------------------------------------------------------------
-- JavaScript FFI
----------------------------------------------------------------------

foreign import javascript unsafe "$r = new Image();"
    jsNewImage :: IO JSC.Image

foreign import javascript unsafe "$1.src = $2;"
    jsSetSrc :: JSC.Image -> MS.MisoString -> IO ()

foreign import javascript unsafe "$r = mycanvas.getContext('2d');"
    jsGetCtx :: IO JSC.Context

foreign import javascript unsafe "$4.drawImage($1, $2, $3);"
    jsDrawImage :: JSC.Image -> Double -> Double -> JSC.Context -> IO ()

foreign import javascript unsafe "console.log($1);"
    jsConsoleLog :: MS.MisoString -> IO ()

