{-# LANGUAGE OverloadedStrings #-}

import Data.Map (singleton)
import GHCJS.Types
import JavaScript.Web.Canvas
import Miso
import Miso.String hiding (singleton)

type Model = (Double, Double)

data Action
  = NoOp
  | GetTime
  | SetTime Model

main :: IO ()
main = do
  earth <- newImage
  setSrc earth "https://mdn.mozillademos.org/files/1429/Canvas_earth.png"
  startApp App 
    { initialAction = GetTime
    , update = updateModel earth
    , view   = \_ -> canvas_ [ id_ "canvas" , width_ "300" , height_ "300"
                             , style_  (singleton "border" "1px solid black")
                             ] []
    , model  = (0.0, 0.0)
    , subs   = []
    , events = defaultEvents
    , mountPoint = Nothing
    }

updateModel :: Image -> Action -> Model -> Effect Action Model
updateModel _ NoOp m = noEff m
updateModel _ GetTime m = m <# do
  date <- newDate
  (s,m') <- (,) <$> getSecs date <*> getMillis date
  pure $ SetTime (s,m')
updateModel earth (SetTime m@(secs,millis)) _ = m <# do
  ctx <- getCtx
  setGlobalCompositeOperation ctx
  clearRect 0 0 300 300 ctx
  -- drawImage earth 0 0 300 300 ctx
  -- drawImageXY earth 100 200 ctx
  drawImageXY earth (0.1 * millis) 200 ctx
  pure GetTime

foreign import javascript unsafe "$1.globalCompositeOperation = 'destination-over';"
  setGlobalCompositeOperation :: Context -> IO ()

foreign import javascript unsafe "$4.drawImage($1,$2,$3);"
  drawImageXY :: Image -> Double -> Double -> Context -> IO ()

foreign import javascript unsafe "$r = document.getElementById('canvas').getContext('2d');"
  getCtx :: IO Context

foreign import javascript unsafe "$r = new Image();"
  newImage :: IO Image

foreign import javascript unsafe "$1.src = $2;"
  setSrc :: Image -> MisoString -> IO ()

foreign import javascript unsafe "$r = new Date();"
  newDate :: IO JSVal

foreign import javascript unsafe "$r = $1.getSeconds();"
  getSecs :: JSVal -> IO Double

foreign import javascript unsafe "$r = $1.getMilliseconds();"
  getMillis :: JSVal -> IO Double


