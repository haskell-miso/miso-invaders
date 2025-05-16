
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad (replicateM_)
-- import Language.Javascript.JSaddle (JSM, (#), fromJSVal, new, jsg)

import Miso
import Miso.Canvas as Canvas
import Miso.String qualified as String
import Miso.Style as Style

import Game

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

type Model = ()

mkModel :: Model
mkModel = ()

type Action = ()

main :: IO ()
main =
  run $ do
    spongebob <- newImage "spongebob.png"
    startComponent (app spongebob) { initialAction = Just () }
  where
    app sb = defaultComponent mkModel handleUpdate (handleView sb)

handleView :: Image -> Model -> View Action
handleView sb model = 
  div_ [] 
    [ p_ [] [ "Usage: left/right to move, space to fire and enter to start..." ]
    , Canvas.canvas_ 
        [ id_ "mycanvas"
        , width_ (String.ms gameWidth)
        , height_ (String.ms gameHeight)
        , Style.style_  [Style.border "1px solid black"]
        ] 
        (canvasDraw sb model)
    , p_ []
         [ a_ [ href_ "https://gitlab.com/juliendehos/miso-invaders"]
              [ text "source code" ]
         , text " / "
         , a_ [ href_ "https://juliendehos.gitlab.io/miso-invaders"]
              [ text "demo" ]
         ]
 ]

canvasDraw :: Image -> Model -> Canvas ()
canvasDraw sb () = do
   globalCompositeOperation DestinationOver
   clearRect (0, 0, 800, 600)
   fillStyle $ Canvas.color (Style.rgba 0 0 0 0.6)
   strokeStyle $ Canvas.color (Style.rgba 0 153 255 0.4)
   drawImage' (sb, 0, 0, 50, 50)

handleUpdate :: Action -> Effect Model Action
handleUpdate () = pure ()

