{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Set qualified as S
import Language.Javascript.JSaddle (JSM, jsg, (#), JSVal, ToJSVal, new, makeObject)
import Control.Lens hiding ((#), view)
import Linear
import Miso hiding ((<#))
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

paddleFilename, touchedFilename, wonFilename, lostFilename :: MS.MisoString
paddleFilename = "spongebob.png"
touchedFilename = "touched.mp3" 
wonFilename = "won.mp3" 
lostFilename = "lost.mp3" 

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

data Resources = Resources
  { _resImagePaddle :: Image
  , _resAudioTouched :: Audio
  , _resAudioWon :: Audio
  , _resAudioLost :: Audio
  }

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

handleView :: Resources -> Model -> View Action
handleView res model = div_ [] 
  [ p_ [] [ "Usage: left/right to move, space to fire and enter to start..." ]
  , Canvas.canvas_ 
      [ id_ "mycanvas"
      , width_ (MS.ms gameWidth)
      , height_ (MS.ms gameHeight)
      , Style.style_  [Style.border "1px solid black"]
      ] 
      (canvasDraw res model)
  , p_ [] [ text ("fps: " <> MS.ms (model^.mFps)) ] 
  , p_ []
       [ a_ [ href_ "https://gitlab.com/juliendehos/miso-invaders"]
            [ text "source code" ]
       , text " / "
       , a_ [ href_ "https://juliendehos.gitlab.io/miso-invaders"]
            [ text "demo" ]
       ]
  ]

canvasDraw :: Resources -> Model -> Canvas ()
canvasDraw res model = do
  globalCompositeOperation DestinationOver
  clearRect (0, 0, gameWidthD, gameHeightD)
  case model^.mGame.status of
    Welcome -> drawText "Welcome ! Press Enter to start..."
    Won     -> drawText "You win !"
    Lost    -> drawText "Game over !"
    Running -> drawGame res (model^.mGame)

drawText :: MS.MisoString -> Canvas ()
drawText txt = do
  fillStyle (color Style.black)
  textAlign TextAlignCenter
  font "40px Arial"
  fillText (txt, 0.5*gameWidthD, 0.5*gameHeightD)

drawItem :: Item -> Canvas ()
drawItem (Item (V2 sx sy) (V2 px py) _) = 
  fillRect (px-0.5*sx, py-0.5*sy, sx, sy)

drawGame :: Resources -> Game -> Canvas ()
drawGame res game = do
  fillStyle (color Style.blue)
  mapM_ drawItem (game^.bullets)
  fillStyle (color Style.red)
  mapM_ drawItem (game^.invaders)
  let (V2 px py) = game^.paddle.pos
  drawImage (_resImagePaddle res, px-0.5*paddleWidth, py-0.5*paddleHeight)

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Resources -> Action -> Effect Model Action

handleUpdate _ ActionReset = do
  mGame %= resetGame
  io (ActionStep <$> myGetTime)

handleUpdate _ (ActionKey keys) = 
  if S.member 13 keys   -- Enter
  then issue ActionReset
  else do
    mGame . inputLeft  .= S.member 37 keys    -- Left arrow
    mGame . inputRight .= S.member 39 keys    -- Right arrow
    mGame . inputFire  .= S.member 32 keys    -- Space

handleUpdate res (ActionStep t1) = do
  t0 <- use mTime
  let dt = t1 - t0
  mGame %= step dt
  mTime .= t1
  touched <- uses mGame _hasTouched
  st <- uses mGame _status
  case (st, touched) of
    (Won, _) -> io_ $ playAudio (_resAudioWon res)
    (Lost, _) -> io_ $ playAudio (_resAudioLost res)
    (Running, False) -> io (ActionStep <$> myGetTime)
    (Running, True) -> io $ do
      playAudio (_resAudioTouched res)
      ActionStep <$> myGetTime
    _ -> pure ()
  mFpsTime += dt
  mFpsTicks += 1
  fpsTime <- use mFpsTime
  when (fpsTime >= 1) $ do
    use mFpsTicks >>= assign mFps
    mFpsTime .= 0
    mFpsTicks .= 0

----------------------------------------------------------------------
-- JavaScript FFI
----------------------------------------------------------------------

newtype Audio = Audio JSVal
  deriving (ToJSVal)

newAudio :: MS.MisoString -> JSM Audio
newAudio url = do
  a <- new (jsg ("Audio" :: MS.MisoString)) ([] :: [MS.MisoString])
  o <- makeObject a
  Miso.set "src" url o
  pure (Audio a)

playAudio :: Audio -> JSM ()
playAudio (Audio a) = void $ a # ("play"::MS.MisoString) $ ()

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

myGetTime :: JSM Double
myGetTime = (* 0.001) <$> now

main :: IO ()
main = run $ do
  res <- Resources 
          <$> newImage paddleFilename 
          <*> newAudio touchedFilename
          <*> newAudio wonFilename
          <*> newAudio lostFilename
  myRands <- take 1000 . randoms <$> newStdGen
  let game = mkGame paddleWidth paddleHeight myRands
  let model = Model game 0 0 0 0
  startComponent Component
    { model = model
    , update = handleUpdate res
    , view = handleView res
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

