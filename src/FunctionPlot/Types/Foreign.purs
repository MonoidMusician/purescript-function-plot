module FunctionPlot.Types.Foreign where

import FunctionPlot.Types
import Prelude (map, show, (<<<), (>>>))

import Data.Foreign (Foreign, toForeign)
import Data.Foreign.NullOrUndefined (undefined)
import Data.Function.Uncurried (Fn3)
import Data.Newtype (class Newtype)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)

toForeignOptions :: ReallyMaxOptions -> Foreign
toForeignOptions opts = toForeign
  { target: toForeign opts.target
  , title: toForeign opts.title
  , xAxis: toForeignAxisOptions opts.xAxis
  , yAxis: toForeignAxisOptions opts.yAxis
  , disableZoom: toForeignEnable opts.disableZoom
  , grid: toForeignEnable opts.grid
  , tip:
    { xLine: toForeignEnable opts.tip.xLine
    , yLine: toForeignEnable opts.tip.yLine
    , renderer: toForeignFn3 opts.tip.renderer
    }
  , annotations: toForeign opts.annotations
  , data: toForeignData opts.data
  }

toForeignEnable :: Newtype Enable Boolean => Enable -> Foreign
toForeignEnable = toForeign

toForeignFn3 :: forall a b c d. Fn3 a b c d -> Foreign
toForeignFn3 = toForeign

toForeignAxisOptions :: Record MaxAxisOptions -> Foreign
toForeignAxisOptions opts = toForeign
  { type: toForeignAxisType opts.type
  , domain: toForeignInterval opts.domain
  , invert: toForeign opts.invert
  , label: toForeign opts.label
  }

toForeignAxisType :: AxisType -> Foreign
toForeignAxisType = toForeign <<< show

toForeignInterval :: Interval -> Foreign
toForeignInterval Default = undefined
toForeignInterval (Interval l r) = toForeign [toForeign l, toForeign r]

toForeignData :: Data -> Foreign
toForeignData = toForeign

mkDatum ::
  forall datum datum' attr.
    Union datum datum' (MaxDataOptions attr) =>
  DatumU datum -> Datum
mkDatum (Datum datum variant) =
  unsafeCoerce case toTup variant of
    Tuple k v ->
      SM.union (toSM datum) (SM.insert "fnType" k v)
  where
    toSM :: forall a r. Record r -> SM.StrMap a
    toSM = unsafeCoerce

    toTup :: forall a s. DatumV -> Tuple s (SM.StrMap a)
    toTup = unsafeCoerce >>> map toSM
