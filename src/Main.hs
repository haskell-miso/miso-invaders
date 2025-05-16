
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad (replicateM_)
-- import Language.Javascript.JSaddle (JSM, (#), fromJSVal, new, jsg)

import Miso
import Miso.Canvas as Canvas
import Miso.String as String
import Miso.Style as Style

import Game

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

----------------------------------------------------------------------
-- params
----------------------------------------------------------------------

paddleWidth, paddleHeight :: Double
paddleWidth = 70
paddleHeight = 66

paddleImgName :: MisoString
paddleImgName = "spongebob.png"

----------------------------------------------------------------------
-- types
----------------------------------------------------------------------

type Model = ()

mkModel :: Model
mkModel = ()

type Action = ()

----------------------------------------------------------------------
-- view handler
----------------------------------------------------------------------

handleView :: Image -> Model -> View Action
handleView paddleImg model = 
  div_ [] 
    [ p_ [] [ "Usage: left/right to move, space to fire and enter to start..." ]
    , Canvas.canvas_ 
        [ id_ "mycanvas"
        , width_ (String.ms gameWidth)
        , height_ (String.ms gameHeight)
        , Style.style_  [Style.border "1px solid black"]
        ] 
        (canvasDraw paddleImg model)
    , p_ []
         [ a_ [ href_ "https://gitlab.com/juliendehos/miso-invaders"]
              [ text "source code" ]
         , text " / "
         , a_ [ href_ "https://juliendehos.gitlab.io/miso-invaders"]
              [ text "demo" ]
         ]
 ]

canvasDraw :: Image -> Model -> Canvas ()
canvasDraw paddleImg () = do
   globalCompositeOperation DestinationOver
   clearRect (0, 0, gameWidthD, gameHeightD)
   -- fillStyle $ Canvas.color (Style.rgba 0 0 0 1.0)
   -- strokeStyle $ Canvas.color (Style.rgba 0 153 255 0.4)
   drawImage' (paddleImg, 0, 0, paddleWidth, paddleHeight)

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Action -> Effect Model Action
handleUpdate () = pure ()

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

main :: IO ()
main =
  run $ do
    paddleImg <- newImage paddleImgName
    let app = defaultComponent mkModel handleUpdate (handleView paddleImg)
    startComponent app { initialAction = Just () }

