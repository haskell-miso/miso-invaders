{-# LANGUAGE StrictData #-}

module Game where

import Control.Lens
import Control.Monad.State
import Data.Bool (bool)
import Data.List (singleton)
import Data.Map qualified as M
import Linear

-------------------------------------------------------------------------------
-- global parameters
-------------------------------------------------------------------------------

gameWidth, gameHeight :: Int
gameWidth = 800
gameHeight = 600

gameWidthD, gameHeightD :: Double
gameWidthD = fromIntegral gameWidth
gameHeightD = fromIntegral gameHeight

-------------------------------------------------------------------------------
-- types
-------------------------------------------------------------------------------

data Status
  = Welcome
  | Running
  | Won
  | Lost
  deriving (Eq)

data Item = Item
  { _siz  :: V2 Double
  , _pos  :: V2 Double
  , _vel  :: V2 Double
  } deriving (Eq)

data Game = Game
  { _status       :: Status
  , _hasTouched   :: Bool
  , _inputLeft    :: Bool
  , _inputRight   :: Bool
  , _inputFire    :: Bool
  , _rands        :: [Double]
  , _fireTime     :: Double
  , _paddle       :: Item
  , _bullets      :: [Item]
  , _invaders     :: [Item]
  , _paddleSize   :: V2 Double
  } deriving (Eq)

-------------------------------------------------------------------------------
-- lenses
-- (compile time is much longer with makeLenses)
-------------------------------------------------------------------------------

{-
makeLenses ''Item
makeLenses ''Game
-}

siz, pos, vel :: Lens' Item (V2 Double)
siz f o = (\x' -> o {_siz = x'}) <$> f (_siz o)
pos f o = (\x' -> o {_pos = x'}) <$> f (_pos o)
vel f o = (\x' -> o {_vel = x'}) <$> f (_vel o)

status :: Lens' Game Status
status f o = (\x' -> o {_status = x'}) <$> f (_status o)

hasTouched, inputLeft, inputRight, inputFire :: Lens' Game Bool
hasTouched f o = (\x' -> o {_hasTouched = x'}) <$> f (_hasTouched o)
inputLeft f o = (\x' -> o {_inputLeft = x'}) <$> f (_inputLeft o)
inputRight f o = (\x' -> o {_inputRight = x'}) <$> f (_inputRight o)
inputFire f o = (\x' -> o {_inputFire = x'}) <$> f (_inputFire o)

rands :: Lens' Game [Double]
rands f o = (\x' -> o {_rands = x'}) <$> f (_rands o)

fireTime :: Lens' Game Double
fireTime f o = (\x' -> o {_fireTime = x'}) <$> f (_fireTime o)

paddle :: Lens' Game Item
paddle f o = (\x' -> o {_paddle = x'}) <$> f (_paddle o)

bullets, invaders :: Lens' Game [Item]
bullets f o = (\x' -> o {_bullets = x'}) <$> f (_bullets o)
invaders f o = (\x' -> o {_invaders = x'}) <$> f (_invaders o)

paddleSize :: Lens' Game (V2 Double)
paddleSize f o = (\x' -> o {_paddleSize = x'}) <$> f (_paddleSize o)

-------------------------------------------------------------------------------
-- simulate an infinite list
-- (for wasm/js backends)
-------------------------------------------------------------------------------

takeCycle :: Int -> [a] -> ([a], [a])
takeCycle n xs = (xs0, xs1++xs0)
  where (xs0, xs1) = splitAt n xs

takeCycleGame :: Int -> State Game [Double]
takeCycleGame n = do
  (xs0, xs1) <- uses rands (splitAt n)
  rands .= xs1++xs0
  pure xs0

-------------------------------------------------------------------------------
-- game main functions
-------------------------------------------------------------------------------

mkGame :: Double -> Double -> [Double] -> Game
mkGame pw ph rands0 = 
  Game Welcome False False False False rands1 0 myPaddle [] myInvaders (V2 pw ph)
  where 
    myPaddle = Item (V2 pw ph) (V2 (gameWidthD/2) (gameHeightD - ph)) (V2 0 0)
    ([mag, dir], rands1) = takeCycle 2 rands0
    vx = (150 + 200 * mag) * (if dir < 0.5 then 1 else -1)
    myInvaders = 
      [ Item (V2 70 20) 
          (V2 (fromIntegral x * 100 + gameWidthD/2) (fromIntegral y * 50 + 20))
          (V2 vx 0)
      | x<-[-2..(2::Int)], y<-[0..(2::Int)] ]

resetGame :: Game -> Game
resetGame game0 = mkGame pw ph rands0 & status .~ Running
  where
    rands0 = game0^.rands
    (V2 pw ph) = game0^.paddleSize

step :: Double -> Game -> Game
step dt g0
  | _status g0 /= Running = g0
  | otherwise = g1 & hasTouched .~ (nb1<nb0)
    where
      update = updateCollisions >> updateBullets dt >> updateInvaders dt >> updatePaddle dt
      g1 = execState update g0
      nb0 = length $ g0^.invaders
      nb1 = length $ g1^.invaders

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

updatePaddle :: Double -> State Game ()
updatePaddle time = do
  let dx = time * 200
  dl <- uses inputLeft $ bool 0 (-dx)
  dr <- uses inputRight $ bool 0 dx
  p0 <- uses paddle _pos
  let x1 = max 0 $ min gameWidthD $ dl + dr + p0^._x
  paddle . pos . _x .= x1
  firePaddleBullet time

firePaddleBullet :: Double -> State Game ()
firePaddleBullet time = do
  isFireInput <- use inputFire
  isFireTime <- uses fireTime (>0.9)
  if isFireInput && isFireTime
  then do
    (V2 x y) <- uses paddle  _pos
    let bullet = Item (V2 3 9) (V2 x (y-40)) (V2 0 (-200))
    bullets %= (bullet:)
    fireTime .= 0
  else fireTime += time 

updateInvaders :: Double -> State Game ()
updateInvaders time = do
  myInvaders <- use invaders
  case myInvaders of
    [] -> pure ()
    (firstInvader:_) -> do
      let
        i1 = map (moveItem time) myInvaders
        xs = map (view (pos . _x)) myInvaders
        x1min = minimum xs
        x1max = maximum xs
        v@(V2 vx _) = firstInvader^.vel
        move v0 v1 i = i & pos +~ time*^v0 & vel .~ v1
        maxx = gameWidthD - 80
        minx = 80
        i2 | vx>0 && x1max>maxx = map (move (V2 (maxx-x1max) 0) (negated v)) i1
           | vx<0 && x1min<minx = map (move (V2 (minx-x1min) 0) (negated v)) i1
           | otherwise = i1
      invaders .= i2
      fireInvadersBullets

fireInvadersBullets :: State Game ()
fireInvadersBullets = do
  invadersPos <- uses invaders (map _pos)
  let 
    fInsert pMap (V2 x y) = M.insertWith max x y pMap
    fighters0 = fmap (uncurry V2) <$> M.toList $ foldl fInsert M.empty invadersPos
  rands0 <- takeCycleGame (length fighters0) 
  rands2 <- takeCycleGame (length fighters0)
  let
    nbInvaders0 = 15
    ratioInvaders = fromIntegral (length invadersPos) / nbInvaders0
    difficulty = 0.95 + 0.04 * ratioInvaders
    fighters1 = [ (p, v) | (p, r, v) <- zip3 fighters0 rands0 rands2, r > difficulty ]
    mkBullet (V2 x y, v) = Item (V2 3 9) (V2 x (y+20)) (V2 0 (300+v*200))
    bs = map mkBullet fighters1
  bullets %= (++bs)

moveItem :: Double -> Item -> Item
moveItem dt i = i & pos +~ dt *^ i^.vel

updateBullets :: Double -> State Game ()
updateBullets time = bullets %= filter isInside . map (moveItem time)
  where isInside b = b^.pos._y < gameHeightD && b^.pos._y > 0

updateCollisions :: State Game ()
updateCollisions = do
  (b1, i1) <- runCollisions <$> use bullets <*> use invaders
  (b2, p2) <- runCollisions b1 . singleton <$> use paddle
  invaders .= i1
  bullets .= b2
  status .= if null i1 then Won else if null p2 then Lost else Running

testCollision :: Item -> Item -> Bool
testCollision (Item as ap _) (Item bs bp _) =
  ((bx0 < ax0 && ax0 < bx1) || (bx0 < ax1 && ax1 < bx1)) &&
  ((by0 < ay0 && ay0 < by1) || (by0 < ay1 && ay1 < by1))
  where 
    (V2 ax0 ay0) = ap - 0.5 * as
    (V2 ax1 ay1) = ap + 0.5 * as
    (V2 bx0 by0) = bp - 0.5 * bs
    (V2 bx1 by1) = bp + 0.5 * bs

runCollisions :: [Item] -> [Item] -> ([Item], [Item])
runCollisions [] is = ([], is)
runCollisions (b:bs) is = (bs1++bs2, is2)
  where 
    is1 = filter (not . testCollision b) is
    bs1 = [b | length is1 == length is]
    (bs2, is2) = runCollisions bs is1

