{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens hiding ((#), view)
import Control.Monad (when)
import Data.IntSet qualified as S
import Linear
import Miso.Html.Property
import Miso.Html
import Miso hiding ((<#), status, (!!), newStdGen)
import Miso.Canvas as Canvas
import Miso.Media as Media
import Miso.String qualified as MS
import Miso.CSS qualified as Style
import System.Random (newStdGen, randoms)

import Game
import Model

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

playlist :: [MS.MisoString]
playlist =
  [ "kids-game-gaming-background-music-297733.mp3"
  , "puzzle-game-bright-casual-video-game-music-249202.mp3"
  , "roblox-minecraft-fortnite-video-game-music-299145.mp3"
  ]

----------------------------------------------------------------------
-- types
----------------------------------------------------------------------

data Action 
  = ActionKey S.IntSet
  | ActionReset
  | ActionStep Double 
  | ActionPlaylistNext

data Resources = Resources
  { _resImagePaddle :: Image
  , _resMediaTouched :: Media
  , _resMediaWon :: Media
  , _resMediaLost :: Media
  }


----------------------------------------------------------------------
-- view handler
----------------------------------------------------------------------
handleView :: Resources -> Model -> View Model Action
handleView res model = div_ [] 
  [ p_ [] [ "Usage: left/right to move, space to fire and enter to start..." ]
  , Canvas.canvas 
      [ id_ "mycanvas"
      , width_ (MS.ms gameWidth)
      , height_ (MS.ms gameHeight)
      , Style.style_  [Style.border "1px solid black"]
      ] 
      canvasInit
      (canvasDraw res model)
  , p_ [] [ text ("fps: " <> MS.ms (model^.mFps)) ] 
  , p_ []
       [ a_ [ href_ "https://github.com/haskell-miso/miso-invaders"]
            [ text "source code" ]
       , text " / "
       , a_ [ href_ "https://haskell-miso.github.io/miso-invaders"]
            [ text "demo" ]
       ]
  , audio_ audioAttrs []
  ]
  where
    audioAttrs = case model ^. mIndexPlaylist of
      Nothing -> []
      Just i -> 
        [ volume_ 0.2
        , src_ (playlist !! (i `mod` length playlist))
        , autoplay_ True
        , onEnded ActionPlaylistNext
        ]

canvasInit :: DOMRef -> Canvas ()
canvasInit _ = pure ()

canvasDraw :: Resources -> Model -> () -> Canvas ()
canvasDraw res model _ = do
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

handleUpdate :: Resources -> Action -> Effect parent Model Action

handleUpdate _ ActionReset = do
  mGame %= resetGame
  mIndexPlaylist %= \case
    Nothing -> Just 0
    Just i -> Just i
  io (ActionStep <$> myGetTime)

handleUpdate _ (ActionKey keys) = 
  if S.member 13 keys   -- Enter
  then issue ActionReset
  else do
    mGame . inputLeft  .= S.member 37 keys    -- Left arrow
    mGame . inputRight .= S.member 39 keys    -- Right arrow
    mGame . inputFire  .= S.member 32 keys    -- Space

handleUpdate res (ActionStep t1) = do
  -- update game
  t0 <- use mTime
  let dt = t1 - t0
  mGame %= step dt
  mTime .= t1
  touched <- uses mGame _hasTouched
  st <- uses mGame _status
  case (st, touched) of
    (Won, _) -> io_ $ Media.play (_resMediaWon res)
    (Lost, _) -> io_ $ Media.play (_resMediaLost res)
    (Running, False) -> io (ActionStep <$> myGetTime)
    (Running, True) -> io $ do
      Media.play (_resMediaTouched res)
      ActionStep <$> myGetTime
    _ -> pure ()
  -- update fps
  mFpsTime += dt
  mFpsTicks += 1
  fpsTime <- use mFpsTime
  when (fpsTime >= 1) $ do
    use mFpsTicks >>= assign mFps
    mFpsTime .= 0
    mFpsTicks .= 0

handleUpdate _ ActionPlaylistNext = 
  mIndexPlaylist %= fmap (+1)

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

myGetTime :: IO Double
myGetTime = (* 0.001) <$> now

-----------------------------------------------------------------------------

-- | Smart constructor for an audio @Media@ with 'src' element

newAudio :: MisoString -> IO Media
newAudio url = do
  a <- new (jsg "Audio") ([] :: [MisoString])
  o <- toObject a
  Miso.set ("src" :: MisoString) url o
  pure (Media a)

main :: IO ()
main = do
  res <- Resources 
          <$> newImage paddleFilename
          <*> newAudio touchedFilename
          <*> newAudio wonFilename
          <*> newAudio lostFilename
  myRands <- take 1000 . randoms <$> newStdGen
  let model = mkModel $ mkGame paddleWidth paddleHeight myRands
  startApp (defaultEvents <> mediaEvents)
    (component model (handleUpdate res) (handleView res))
      { subs = [ keyboardSub ActionKey ]
      , logLevel = DebugAll
      }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

