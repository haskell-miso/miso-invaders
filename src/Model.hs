module Model where

import Control.Lens

import Game

data Model = Model
  { _mGame :: Game
  , _mTime :: Double
  , _mFps :: Int
  , _mFpsTime :: Double
  , _mFpsTicks :: Int
  , _mIndexPlaylist :: Maybe Int
  } deriving (Eq)

makeLenses ''Model

mkModel :: Game -> Model
mkModel g = Model g 0 0 0 0 Nothing

