{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

import Control.Monad (void, when)
import Data.Set qualified as S
import Language.Javascript.JSaddle (JSM, jsg, (#), toJSString)
-- import Lens.Micro.Platform hiding (view)
import Control.Lens hiding ((#), view)
import Linear
import Miso
import Miso.Canvas as Canvas
import Miso.String qualified as MS
import Miso.Style qualified as Style
import System.Random (newStdGen, randoms)

import Game

----------------------------------------------------------------------
-- global parameters
----------------------------------------------------------------------

paddleWidth, paddleHeight :: Double
paddleWidth = 70
paddleHeight = 66

paddleImgName :: MS.MisoString
paddleImgName = "spongebob.png"

----------------------------------------------------------------------
-- types
----------------------------------------------------------------------

data Model = Model
  { _mGame :: Game
  , _mTime :: Double
  , _mRands :: [Double]
  } deriving (Eq)

data Action 
  = ActionKey (S.Set Int)
  | ActionReset
  | ActionStep Double 

-------------------------------------------------------------------------------
-- lenses
-- (compile time is much longer with makeLenses)
-------------------------------------------------------------------------------

{-
makeLenses ''Model
-}

mTime :: Lens' Model Double
mTime f o = (\x' -> o {_mTime = x'}) <$> f (_mTime o)

mRands :: Lens' Model [Double]
mRands f o = (\x' -> o {_mRands = x'}) <$> f (_mRands o)

mGame :: Lens' Model Game
mGame f o = (\x' -> o {_mGame = x'}) <$> f (_mGame o)

----------------------------------------------------------------------
-- view handler
----------------------------------------------------------------------

handleView :: Image -> Model -> View Action
handleView paddleImg model = div_ [] 
  [ p_ [] [ "Usage: left/right to move, space to fire and enter to start..." ]
  , Canvas.canvas_ 
      [ id_ "mycanvas"
      , width_ (MS.ms gameWidth)
      , height_ (MS.ms gameHeight)
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
  , p_ [] 
      [ audio_ [ id_ "myAudioTouched", src_ "touched.mp3" ] []
      , audio_ [ id_ "myAudioWon", src_ "won.mp3" ] []
      , audio_ [ id_ "myAudioLost", src_ "lost.mp3" ] []
      ]
  ]

canvasDraw :: Image -> Model -> Canvas ()
canvasDraw paddleImg model = do
  globalCompositeOperation DestinationOver
  clearRect (0, 0, gameWidthD, gameHeightD)
  case model^.mGame.status of
    Welcome -> drawText "Welcome ! Press Enter to start..."
    Won     -> drawText "You win !"
    Lost    -> drawText "Game over !"
    Running -> drawGame paddleImg (model^.mGame)

drawText :: MS.MisoString -> Canvas ()
drawText txt = do
  fillStyle (color Style.black)
  textAlign TextAlignCenter
  font "40px Arial"
  fillText (txt, 0.5*gameWidthD, 0.5*gameHeightD)

drawItem :: Item -> Canvas ()
drawItem (Item (V2 sx sy) (V2 px py) _) = 
  fillRect (px-0.5*sx, py-0.5*sy, sx, sy)

drawGame :: Image -> Game -> Canvas ()
drawGame paddleImg game = do
  fillStyle (color Style.blue)
  mapM_ drawItem (game^.bullets)
  fillStyle (color Style.red)
  mapM_ drawItem (game^.invaders)
  let (V2 px py) = game^.paddle.pos
  drawImage (paddleImg, px-0.5*paddleWidth, py-0.5*paddleHeight)

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Action -> Effect Model Action

handleUpdate ActionReset = do
  -- TODO state
  m <- get
  let rands' = doCycle 1 (_mRands m)
      game' = mkGame True rands' paddleWidth paddleHeight
      m' = m { _mGame = game', _mRands = rands' }
  put m'
  io (ActionStep <$> myGetTime)

handleUpdate (ActionKey keys) = 
  if S.member 13 keys
  then issue ActionReset
  else do
    mGame . inputLeft  .= S.member 37 keys
    mGame . inputRight .= S.member 39 keys
    mGame . inputFire  .= S.member 32 keys

handleUpdate (ActionStep t1) = do
  t0 <- use mTime
  let dt = t1 - t0
  mGame %= step dt
  mTime .= t1
  hasTouched <- uses mGame _hasTouched
  st <- uses mGame _status
  case (st, hasTouched) of
    (Won, _) -> io_ $ jsPlayAudio "myAudioWon"
    (Lost, _) -> io_ $ jsPlayAudio "myAudioLost"
    (Running, False) -> io (ActionStep <$> myGetTime)
    (Running, True) -> io (jsPlayAudio "myAudioTouched" >> ActionStep <$> myGetTime)

----------------------------------------------------------------------
-- JavaScript FFI
----------------------------------------------------------------------

jsPlayAudio :: String -> JSM ()
jsPlayAudio name = void $ jsg name # ("play"::String) $ ()

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

myGetTime :: JSM Double
myGetTime = (* 0.001) <$> now

main :: IO ()
main = run $ do
  paddleImg <- newImage paddleImgName
  time <- myGetTime
  -- TODO rands <- randoms <$> newStdGen
  rands <- take 1000 . randoms <$> newStdGen
  let game = mkGame False rands paddleWidth paddleHeight
  startComponent Component
    { model = Model game time rands
    , update = handleUpdate
    , view = handleView paddleImg
    , subs = [ keyboardSub ActionKey ]
    , events = defaultEvents
    , styles = []
    , mountPoint = Nothing
    , logLevel = Off
    , initialAction = Nothing
    }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

