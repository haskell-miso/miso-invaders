{-# LANGUAGE OverloadedStrings #-}

module Audio where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Language.Javascript.JSaddle -- (JSM, jsg, (#), JSVal, ToJSVal, new, makeObject, fromJSVal, (!))
import Miso hiding ((<#))
import Miso.String qualified as MS

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

setVolumeAudio :: Audio -> Double -> JSM ()
setVolumeAudio (Audio a) = a <# ("volume"::MS.MisoString) 

pausedAudio :: Audio -> JSM Bool
pausedAudio (Audio a) = do
  value <- a ! ("paused"::MS.MisoString)
  fromMaybe False <$> fromJSVal value

