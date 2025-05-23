{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Set qualified as S
import Language.Javascript.JSaddle (JSM, jsg, (#))
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

idAudioTouched, idAudioWon, idAudioLost :: MS.MisoString
idAudioTouched = "myAudioTouched" 
idAudioWon = "myAudioWon" 
idAudioLost = "myAudioLost" 

----------------------------------------------------------------------
-- types
----------------------------------------------------------------------

data Model = Model
  { _mGame :: Game
  , _mTime :: Double
  , _mFps :: Int
  , _mFpsTime :: Double
  , _mFpsTicks :: Int
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

mGame :: Lens' Model Game
mGame f o = (\x' -> o {_mGame = x'}) <$> f (_mGame o)

mTime :: Lens' Model Double
mTime f o = (\x' -> o {_mTime = x'}) <$> f (_mTime o)

mFps :: Lens' Model Int
mFps f o = (\x' -> o {_mFps = x'}) <$> f (_mFps o)

mFpsTime :: Lens' Model Double
mFpsTime f o = (\x' -> o {_mFpsTime = x'}) <$> f (_mFpsTime o)

mFpsTicks :: Lens' Model Int
mFpsTicks f o = (\x' -> o {_mFpsTicks = x'}) <$> f (_mFpsTicks o)

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
  , p_ [] [ text ("fps: " <> MS.ms (model^.mFps)) ] 
  , p_ []
       [ a_ [ href_ "https://gitlab.com/juliendehos/miso-invaders"]
            [ text "source code" ]
       , text " / "
       , a_ [ href_ "https://juliendehos.gitlab.io/miso-invaders"]
            [ text "demo" ]
       ]
  , p_ [] 
      [ audio_ [ id_ idAudioTouched, src_ "touched.mp3" ] []
      , audio_ [ id_ idAudioWon, src_ "won.mp3" ] []
      , audio_ [ id_ idAudioLost, src_ "lost.mp3" ] []
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
  mGame %= resetGame
  io (ActionStep <$> myGetTime)

handleUpdate (ActionKey keys) = 
  if S.member 13 keys   -- Enter
  then issue ActionReset
  else do
    mGame . inputLeft  .= S.member 37 keys    -- Left arrow
    mGame . inputRight .= S.member 39 keys    -- Right arrow
    mGame . inputFire  .= S.member 32 keys    -- Space

handleUpdate (ActionStep t1) = do
  t0 <- use mTime
  let dt = t1 - t0
  mGame %= step dt
  mTime .= t1
  touched <- uses mGame _hasTouched
  st <- uses mGame _status
  case (st, touched) of
    (Won, _) -> io_ $ jsPlayAudio idAudioWon
    (Lost, _) -> io_ $ jsPlayAudio idAudioLost
    (Running, False) -> io (ActionStep <$> myGetTime)
    (Running, True) -> io (jsPlayAudio idAudioTouched >> ActionStep <$> myGetTime)
    _ -> pure ()
  mFpsTime += dt
  mFpsTicks += 1
  fpsTime <- use mFpsTime
  when (fpsTime > 1) $ do
    ticks <- use mFpsTicks
    mFps .= ticks
    mFpsTime .= 0
    mFpsTicks .= 0

----------------------------------------------------------------------
-- JavaScript FFI
----------------------------------------------------------------------

jsPlayAudio :: MS.MisoString -> JSM ()
jsPlayAudio name = void $ jsg name # ("play"::MS.MisoString) $ ()

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

myGetTime :: JSM Double
myGetTime = (* 0.001) <$> now

main :: IO ()
main = run $ do
  paddleImg <- newImage paddleImgName
  myRands <- take 1000 . randoms <$> newStdGen
  let game = mkGame paddleWidth paddleHeight myRands
  startComponent Component
    { model = Model game 0 0 0 0
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

