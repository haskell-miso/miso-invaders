{-# LANGUAGE CPP #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad (replicateM_)
-- import Language.Javascript.JSaddle (JSM, (#), fromJSVal, new, jsg)

import Miso
import Miso.Canvas
import qualified Miso.Canvas as Canvas
-- import Miso.String
import Miso.Style

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
    [ p_ [] ["foobar"]
    -- , div_ 
    --     [ id_ "canvas grid" ] 
    --     [ Canvas.canvas_ 
    --       [ id_ "canvas", width_ "800", height_ "600" ] (canvasDraw sb model)
    --     ]
    ]

canvasDraw :: Image -> Model -> Canvas ()
canvasDraw sb () = do
   globalCompositeOperation DestinationOver
   clearRect (0, 0, 800, 600)
   fillStyle $ Canvas.color (rgba 0 0 0 0.6)
   strokeStyle $ Canvas.color (rgba 0 153 255 0.4)
   drawImage' (sb, 0, 0, 50, 50)

handleUpdate :: Action -> Effect Model Action
handleUpdate () = pure ()

