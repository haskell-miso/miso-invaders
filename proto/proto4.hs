{-# LANGUAGE OverloadedStrings #-}

import Data.Map (singleton)
import JavaScript.Web.Canvas
import Miso
import Miso.String hiding (singleton)

data Model = Model 
    { _x :: Double
    , _y :: Double
    , _rands :: [Double]
    } deriving (Eq)

data Action
  = NoOp
  | GetTime
  | SetTime

main :: IO ()
main = do
  earth <- newImage
  setSrc earth "https://mdn.mozillademos.org/files/1429/Canvas_earth.png"
  startApp App 
    { initialAction = GetTime
    , update = updateModel earth
    , view   = \_ -> canvas_ [ id_ "mycanvas" , width_ "300" , height_ "300"
                             , style_  (singleton "border" "1px solid black")
                             ] []
    , model  = Model 100 200 [1..]
    -- , model  = Model 100 200 [1..1000]
    , subs   = []
    , events = defaultEvents
    , mountPoint = Nothing
    }

updateModel :: Image -> Action -> Model -> Effect Action Model
updateModel _ NoOp m = noEff m
updateModel _ GetTime m = m { _x = x' } <# pure SetTime
    where x = _x m
          x' = if x > 200 then 100 else x + 1
updateModel earth SetTime m = m <# do
  ctx <- getCtx
  clearRect 0 0 300 300 ctx
  drawImageXY earth (_x m) (_y m) ctx
  pure GetTime

foreign import javascript unsafe "$4.drawImage($1,$2,$3);"
  drawImageXY :: Image -> Double -> Double -> Context -> IO ()

foreign import javascript unsafe "$r = mycanvas.getContext('2d');"
  getCtx :: IO Context

foreign import javascript unsafe "$r = new Image();"
  newImage :: IO Image

foreign import javascript unsafe "$1.src = $2;"
  setSrc :: Image -> MisoString -> IO ()

