module FunctionPlot.Types.Foreign where

import Data.Variant (SProxy(..), Variant, case_, on)
import FunctionPlot.Types
import Prelude (map, show, (#), ($), (<<<), (>>>))

import Data.Bifunctor (bimap)
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Class (encode)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), undefined)
import Data.Function.Uncurried (Fn3)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Record (get, insert)
import Data.StrMap as SM
import Data.Symbol (reflectSymbol)
import Type.Prelude (class IsSymbol)
import Type.Row (class RowLacks, class RowToList, Cons, Nil)
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

toForeignPoints :: Points -> Foreign
toForeignPoints = toForeign <<< map toForeignPoint

toForeignPoint :: Point -> Foreign
toForeignPoint (Point x y) = toForeign [toForeign x, toForeign y]

class RowSingleton
  (k :: Symbol)
  (v :: Type)
  (r :: # Type)
  | r -> k v where
  onekey :: Record r -> SProxy k
  onevalue :: Record r -> v
  onemap ::
    forall v' r'.
      RowCons k v' () r' =>
      RowLacks k () =>
    (v -> v') -> Record r -> Record r'

instance rowSingleton ::
  ( IsSymbol k
  , RowCons k v () r
  , RowToList r (Cons k v Nil)
  ) => RowSingleton k v r
  where
    onekey _ = SProxy
    onevalue r = get (onekey r) r
    onemap f r = insert (SProxy :: SProxy k) (f (onevalue r)) {}

mkDatum ::
  forall datum datum' attr.
    Union datum datum' (MaxDataOptions attr) =>
  DatumU datum -> Datum
mkDatum (Datum datum variant) = (unsafeCoerce :: SM.StrMap Foreign -> Datum)
    (SM.union gt (SM.union foreignized (handleAll variant)))
  where
    gt = maybe SM.empty (unsafeCoerce case _ of
      IntervalGraph -> toSM { graphType: toForeign "interval" }
      PolylineGraph _ _ -> toSM { graphType: toForeign "polyline" }
      ScatterGraph _ _ -> toSM { graphType: toForeign "scatter" }) $
        SM.lookup "graphType" (toSM datum)
    nullable :: forall a. (a -> Foreign) -> Maybe a -> Foreign
    nullable f a = encode (NullOrUndefined (map f a))
    onKey :: forall a. String -> (a -> Foreign) -> SM.StrMap Foreign -> SM.StrMap Foreign
    onKey k f = SM.alter (map (unsafeCoerce f :: Foreign -> Foreign)) k
    foreignized = datum # toSM
      # onKey "range" toForeignInterval
      # onKey "derivative" (nullable (toForeignDerivative))
      # onKey "secants" (toForeign <<< map toForeignSplitUpdatePolicy :: Array Secant -> Array Foreign)
      # map toForeign

    toSM :: forall r. Record r -> SM.StrMap Foreign
    toSM = unsafeCoerce

    oneForeign ::
      forall k v r.
        RowSingleton k v r =>
        IsSymbol k =>
      (v -> Foreign) -> Record r -> SM.StrMap Foreign
    oneForeign f r =
      SM.singleton (reflectSymbol (onekey r)) $
        f $ onevalue r

    handle ::
      forall k v meh n.
        IsSymbol k =>
        RowCons k v meh n =>
      SProxy k -> (v -> SM.StrMap Foreign) ->
      (Variant meh -> SM.StrMap Foreign) -> Variant n -> SM.StrMap Foreign
    handle key f =
      on key \rec ->
        SM.insert "fnType" (toForeign (reflectSymbol key)) ((f rec))

    handleAll :: DatumV -> SM.StrMap Foreign
    handleAll = case_
      # handle (SProxy :: SProxy "linear") (oneForeign toForeignFunctionValue)
      # handle (SProxy :: SProxy "polar") (oneForeign toForeignFunctionValue)
      # handle (SProxy :: SProxy "implicit") (oneForeign toForeignFunctionValue2)
      # handle (SProxy :: SProxy "points") (oneForeign toForeignPoints)
      # handle (SProxy :: SProxy "parametric")
        (\{ x, y} -> toSM { x: toForeignFunctionValue x, y: toForeignFunctionValue y })
      # handle (SProxy :: SProxy "vector") \{ vector, offset } ->
        case offset of
          Nothing -> oneForeign toForeignPoint { vector }
          Just o -> toSM { vector: toForeignPoint vector, offset: toForeignPoint o }

toForeignDerivative :: Derivative -> Foreign
toForeignDerivative =
  bimap toForeignD toForeignD >>> toForeignSplitUpdatePolicy
  where
    toForeignD :: forall r s. { fn :: FunctionValue s | r } -> { fn :: Foreign | r }
    toForeignD r@{ fn } = r { fn = toForeignFunctionValue fn }

toForeignSplitUpdatePolicy ::
  forall w wo.
    RowLacks "updateOnMouseMove" w =>
    RowLacks "updateOnMouseMove" wo =>
  SplitUpdatePolicy (Record w) (Record wo) -> Foreign
toForeignSplitUpdatePolicy = case _ of
  At w -> toForeign w
  StartAt w -> toForeign (updateOnMouseMove w)
  Updating wo -> toForeign (updateOnMouseMove wo)
  where
    updateOnMouseMove ::
      forall r.
        RowLacks "updateOnMouseMove" r =>
      Record r -> { updateOnMouseMove :: Boolean | r }
    updateOnMouseMove = insert (SProxy :: SProxy "updateOnMouseMove") true

toForeignFunctionValue :: forall s. FunctionValue s -> Foreign
toForeignFunctionValue = case _ of
    StrFn s -> toForeign s
    FnVFn f -> toForeign f

toForeignFunctionValue2 :: forall s z. FunctionValue2 s z -> Foreign
toForeignFunctionValue2 = case _ of
    StrFn2 s -> toForeign s
    FnVFn2 f -> toForeign f
