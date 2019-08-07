{-# LANGUAGE OverloadedStrings #-}

-- TODO display game
-- TODO keyboard events
-- TODO game over
-- TODO random numbers
-- TODO audio
-- TODO step time
-- TODO flip paddle ?

-- http://hackage.haskell.org/package/ghcjs-base

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

newtype Model = Model
    { _game :: G.Game
    } deriving (Eq)

data Action
    = ActionNone
    | ActionUpdate -- Double
    | ActionKey (S.Set Int)

----------------------------------------------------------------------
-- main function
----------------------------------------------------------------------

main :: IO ()
main = do
    paddleImg <- jsNewImage
    jsSetSrc paddleImg paddleImgName
    -- myRands <- randoms <$> newStdGen
    myRands <- take 1000 . randoms <$> newStdGen
    let game0 = G.createGame myRands paddleWidth paddleHeight
    startApp App 
        { initialAction = ActionUpdate
        , update        = updateModel paddleImg
        , view          = viewModel
        , model         = Model game0
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
drawItem ctx (G.Item (sx, sy) (px, py) _) = JSC.fillRect px py sx sy ctx

drawGame :: JSC.Image -> G.Game -> IO ()
drawGame paddleImg game = do
    ctx <- jsGetCtx
    JSC.clearRect 0 0 G.gameWidthD G.gameHeightD ctx
    JSC.lineWidth 0 ctx
    JSC.fillStyle 255 0 0 255 ctx
    mapM_ (drawItem ctx) invaders
    JSC.stroke ctx
    jsDrawImage paddleImg x y ctx
    where paddle = G._paddle game
          (x, y) = G._pos paddle
          invaders = G._invaders game

----------------------------------------------------------------------
-- update function
----------------------------------------------------------------------

updateModel :: JSC.Image -> Action -> Model -> Effect Action Model

updateModel _ ActionNone m = noEff m

updateModel paddleImg ActionUpdate m = m { _game = g' } <# do
    drawGame paddleImg g
    pure ActionNone
    where g = _game m
          p = G._paddle g
          (x, y) = G._pos p
          x' = if x > G.gameWidthD then 0 else x + 2
          p' = p { G._pos = (x', y) }
          g' = g { G._paddle = p' }

updateModel _ (ActionKey ks) m = 
    m <# return (if S.member 39 ks then ActionUpdate else ActionNone)

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

