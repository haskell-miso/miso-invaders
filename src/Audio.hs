{-# LANGUAGE OverloadedStrings #-}

module Audio where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Miso
import Miso.String qualified as MS

----------------------------------------------------------------------
-- JavaScript FFI
----------------------------------------------------------------------

newtype Audio = Audio JSVal
  deriving (ToJSVal)

newAudio :: MS.MisoString -> IO Audio
newAudio url = do
  a <- new (jsg ("Audio" :: MS.MisoString)) ([] :: [MS.MisoString])
  o <- toObject a
  Miso.set "src" url o
  pure (Audio a)

playAudio :: Audio -> IO ()
playAudio (Audio a) = void $ a # ("play"::MS.MisoString) $ ()

setVolumeAudio :: Audio -> Double -> IO ()
setVolumeAudio (Audio a) = setField a ("volume"::MS.MisoString) 

pausedAudio :: Audio -> IO Bool
pausedAudio (Audio a) = do
  value <- a ! ("paused"::MS.MisoString)
  fromMaybe False <$> fromJSVal value

