module FunctionPlot.API where

import FunctionPlot.Types (Data, MaxAxisOptions, MaxOptions, Plot, defaultOptions)
import FunctionPlot.Types.Foreign (toForeignOptions)
import Prelude (Unit, (<<<))

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Foreign (Foreign)
import FunctionPlot.Types.Internal (class FillIn, fillIn)

type Effects eff a = Eff (dom :: DOM | eff) a

foreign import functionPlotImpl ::
  forall eff. Foreign -> Effects eff Plot
functionPlot ::
  forall r eff.
    FillIn r (Record (MaxOptions (Record MaxAxisOptions) (Record MaxAxisOptions))) =>
  r -> Effects eff Plot
functionPlot = functionPlotImpl <<< toForeignOptions <<< fillIn defaultOptions

foreign import setOptionsImpl ::
  forall eff. Plot -> Foreign -> Effects eff Unit
setOptions ::
  forall r eff.
    FillIn r (Record (MaxOptions (Record MaxAxisOptions) (Record MaxAxisOptions))) =>
  Plot -> r -> Effects eff Unit
setOptions p = setOptionsImpl p <<< toForeignOptions <<< fillIn defaultOptions

foreign import setDataImpl ::
  forall eff. Plot -> Data -> Effects eff Unit
setData ::
  forall eff.
  Plot -> Data -> Eff (dom :: DOM | eff) Unit
setData = setDataImpl

foreign import draw :: forall eff. Plot -> Effects eff Unit
