{-# LANGUAGE OverloadedStrings #-}

import Miso
import Miso.String hiding (head, tail, null)
import System.Random (newStdGen, randoms)

data Model = Model 
    { _x :: Int
    , _rs :: [Double]
    } deriving (Eq)

data Action
    = NoOp
    | GetTime
    | SetTime

main :: IO ()
main = do
    let xs = [1..100]
    -- xs <- randoms <$> newStdGen
    startApp App 
        { initialAction = GetTime
        , update = updateModel
        , view   = viewModel
        , model  = Model 0 xs
        , subs   = []
        , events = defaultEvents
        , mountPoint = Nothing
        }

viewModel :: Model -> View Action
viewModel m = p_ [] [ text (if null rs then "empty" else ms (head rs)) ]
    where rs = _rs m

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel GetTime m = m { _rs = tail (_rs m) } <# pure SetTime
updateModel SetTime m = m <# pure GetTime

