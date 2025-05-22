module Game where

import Control.Lens
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
  { _siz :: !(V2 Double)
  , _pos :: !(V2 Double)
  , _vel :: !(V2 Double)
  } deriving (Eq)

data Game = Game
  { _status :: !Status
  , _hasTouched :: !Bool
  , _inputLeft :: !Bool
  , _inputRight :: !Bool
  , _inputFire :: !Bool
  , _rands :: [Double]    -- infinite lazy list
  , _fireTime :: !Double
  , _paddle :: !Item
  , _bullets :: ![Item]
  , _invaders :: ![Item]
  , _paddleSize :: !(V2 Double)
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
-- 
-------------------------------------------------------------------------------

doCycle :: Int -> [a] -> [a]
doCycle n xs = let (xs0, xs1) = splitAt n xs in xs1++xs0

getCycle :: Int -> [a] -> ([a], [a])
getCycle n xs = let (xs0, xs1) = splitAt n xs in (xs0, xs1++xs0)

mkGame :: Bool -> [Double] -> Double -> Double -> Game
mkGame isRunning rands0 pw ph = 
  Game status False False False False rands1 0 myPaddle [] myInvaders (V2 pw ph)
  where 
    status = if isRunning then Running else Welcome
    myPaddle = Item (V2 pw ph) (V2 (gameWidthD/2) (gameHeightD - ph)) (V2 0 0)
    ([mag, dir], rands1) = getCycle 2 rands0
    vx = (150 + 200 * mag) * (if dir < 0.5 then 1 else -1)
    myInvaders = [ Item (V2 70 20) 
                        (V2 (fromIntegral x * 100 + gameWidthD/2) (fromIntegral y * 50 + 20))
                        (V2 vx 0)
                   | x<-[-2..(2::Int)], y<-[0..(2::Int)] ]

resetGame :: Game -> Game
resetGame game0 =
  let rands1 = doCycle 1 (game0^.rands)
      (V2 pw ph) = game0^.paddleSize
  in mkGame True rands1 pw ph

updatePaddle :: Double -> Game -> Game
updatePaddle time g = firePaddleBullet time g1
  where 
    dx = time * 200
    dl = if _inputLeft g then -dx else 0
    dr = if _inputRight g then dx else 0
    p0 = _paddle g
    (V2 x0 y) = _pos p0
    x1 = max 0 $ min gameWidthD $ dl + dr + x0
    p1 = p0 { _pos = V2 x1 y }
    -- p1 = p0 & pos . _y .= x1 
    g1 = g { _paddle = p1 }

firePaddleBullet :: Double -> Game -> Game
firePaddleBullet time g = if canFire then mFire else mNofire
  where
  canFire = _inputFire g && _fireTime g > 0.9
  (V2 x y) = _pos $ _paddle g
  bullet = Item (V2 3 9) (V2 x (y-40)) (V2 0 (-200))
  mFire = g { _bullets = bullet : _bullets g, _fireTime = 0 }
  mNofire = g { _fireTime = time + _fireTime g }

updateInvaders :: Double -> Game -> Game
updateInvaders time g = if null myInvaders then g else g3
  where 
    myInvaders = _invaders g
    i1 = map (autoUpdateItem time) myInvaders
    xs = map (view (pos . _x)) myInvaders
    x1min = minimum xs
    x1max = maximum xs
    v@(V2 vx _) = _vel $ head myInvaders
    move v0 v1 i = i { _pos = _pos i + time *^ v0, _vel = v1 }
    maxx = gameWidthD - 80
    minx = 80
    i2 | vx>0 && x1max>maxx = map (move (V2 (maxx-x1max) 0) (negated v)) i1
       | vx<0 && x1min<minx = map (move (V2 (minx-x1min) 0) (negated v)) i1
       | otherwise = i1
    g2 = g { _invaders = i2 }
    g3 = fireInvadersBullets g2

fireInvadersBullets :: Game -> Game
fireInvadersBullets g = g { _bullets = _bullets g ++ bs, _rands = rands3 }
  where 
    invadersPos = map _pos $ _invaders g
    fInsert pMap (V2 x y) = M.insertWith max x y pMap
    fighters0 = fmap (uncurry V2) <$> M.toList $ foldl fInsert M.empty invadersPos
    (rands0, rands1) = getCycle (length fighters0) (_rands g)
    (rands2, rands3) = getCycle (length fighters0) rands1
    nbInvaders0 = 15
    ratioInvaders = fromIntegral (length invadersPos) / nbInvaders0
    difficulty = 0.95 + 0.04 * ratioInvaders
    fighters1 = [ (p, v) | (p, r, v) <- zip3 fighters0 rands0 rands2, r > difficulty ]
    mkBullet (V2 x y, v) = Item (V2 3 9) (V2 x (y+20)) (V2 0 (300+v*200))
    bs = map mkBullet fighters1

autoUpdateItem :: Double -> Item -> Item
autoUpdateItem t i@(Item _ p v) = i { _pos = p + t *^ v }

updateBullets :: Double -> Game -> Game
updateBullets time g = g { _bullets = b2 }
  where 
    b1 = map (autoUpdateItem time) (_bullets g)
    b2 = filter (\b -> b^.pos._y < gameHeightD && b^.pos._y > 0) b1

updateCollisions :: Game -> Game
updateCollisions g = g1 { _invaders = i1, _bullets = b2, _status = st }
  where 
    (b1, i1) = runCollisions (_bullets g) (_invaders g)
    (b2, p2) = runCollisions b1 [_paddle g]
    st | null i1 = Won 
       | null p2 = Lost
       | otherwise = Running
    g1 = if st == Running then g 
         else g { _inputLeft = False, _inputRight = False, _inputFire = False }

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

step :: Double -> Game -> Game
step time g0 = 
  if _status g0 /= Running 
  then g0 
  else 
    let nb0 = length $ g0^.invaders
        g1 = updatePaddle time
              $ updateInvaders time 
              $ updateBullets time
              $ updateCollisions g0
        nb1 = length $ g1^.invaders
    in g1 & hasTouched .~ (nb1<nb0)

