{-# LANGUAGE OverloadedStrings #-}

module Audio where

import Language.Javascript.JSaddle -- (JSM, jsg, (#), JSVal, ToJSVal, new, makeObject, fromJSVal, (!))
import Miso.Media
import Miso.String qualified as MS

setVolume :: Media -> Double -> JSM ()
setVolume (Media v) = v <# ("volume"::MS.MisoString) 

