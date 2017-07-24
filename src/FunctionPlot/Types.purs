module FunctionPlot.Types where

import Prelude

import Color (Color)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Function.Uncurried (Fn2, Fn3, mkFn3)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Disj (Disj)
import Data.Newtype (class Newtype, over)
import Data.Ord.Max (Max)
import Data.Ord.Min (Min)
import Data.Record as Rec
import Data.Variant (Variant)
import Type.Prelude (class IsSymbol, SProxy(..))
import Type.Row as R
import Unsafe.Coerce (unsafeCoerce)

foreign import data Plot :: Type

unsafeFillIn :: forall p f. FillIn p f => p -> f
unsafeFillIn = fillIn (unsafeCoerce {})

-- | A typeclass that allows defaulting of options, where options that are not
-- | specified in `partial` are filled in with defaults. For each value that
-- | *does* exist in `partial`, it must either have the same type as the
-- | corresponding row in `filledIn` or be a `Maybe` wrapper around that type.
-- (flip fillIn {}) == id
class FillIn
  (partial :: Type) -- Record
  (filledIn :: Type) -- Record
  where
    fillIn :: filledIn -> partial -> filledIn

instance fillInInstance ::
    -- partial :: Row <-> parts :: RowList
  ( R.RowToList partial parts
  , R.ListToRow parts partial
    -- filledIn :: Row <-> defaultL :: RowList
  , R.RowToList filledIn defaultL
  , R.ListToRow defaultL filledIn
    -- ensure we can instance match against the first row of parts
  , ShuffleNextToTop parts defaultL dL
  , FillInImpl parts partial dL filledIn filledIn
  ) => FillIn (Record partial) (Record filledIn)
  where
    fillIn dfs part = fillInImpl
      (R.RLProxy :: R.RLProxy parts)
      (R.RLProxy :: R.RLProxy dL)
      part dfs

-- | If there is a label Cons'ed on top of `iter`, ensure it is on top of `o`
-- | too (where `i` and `o` represent equivalent rows). This exposes the type
-- | of the matching row of `i` in the head of `o` for instance matching.
class ShuffleNextToTop
  (iter :: R.RowList)
  (i :: R.RowList)
  (o :: R.RowList)
  | iter -> i o
instance noShuffle :: ShuffleNextToTop R.Nil i i
instance shuffleNext ::
    -- convert i to row ir
  ( R.ListToRow i ir
    -- remove symbol
  , RowCons sym t2 or ir
    -- convert to rowlist o
  , R.RowToList or o
    -- and add back on top
  ) => ShuffleNextToTop (R.Cons sym t1 rest) i (R.Cons sym t2 o)

-- | Expands iteration over `RowLists` to build the result options.
class FillInImpl
  (iter :: R.RowList)
  (partial :: # Type)
  (defaultL :: R.RowList)
  (defaults :: # Type)
  (result :: # Type)
  | iter defaultL -> defaults partial result
  , iter -> partial, defaultL -> defaults
  where
    fillInImpl ::
      R.RLProxy iter -> R.RLProxy defaultL ->
        Record partial -> Record defaults -> Record result

-- | If there is no more explicit options left, return what remainds of the
-- | defaults (not including any consumed explicit options).
instance fillInNil ::
  FillInImpl R.Nil () defaultL remaining remaining where
    fillInImpl _ _ _{-{}-} dfs = dfs

-- | If a partial option matches the type of the full option, insert it into
-- | the result.
instance fillInConsZExists ::
    -- ensure it exists in the result
  ( RowCons sym t defaults' defaults
    -- recurse through the remaining keys, subtracting from defaults
    -- and building a subresult
  , FillInImpl rest partial' dL' defaults' result'
  , ShuffleNextToTop rest dL dL'
    -- at this key from partial to the result
  , RowCons sym t result' result
  , RowCons sym t partial' partial
  , IsSymbol sym
  , R.RowLacks sym partial'
  , R.RowLacks sym defaults'
  , R.RowLacks sym result'
  ) => FillInImpl (R.Cons sym t rest) partial (R.Cons sym t dL) defaults result
  where
    fillInImpl _ _ part dfs = Rec.insert key val recurse
      where
        key = SProxy :: SProxy sym
        val :: t
        val = Rec.get key part
        subpart :: Record partial'
        subpart = Rec.delete key (part :: Record partial)
        fillInImpl' :: Record partial' -> Record defaults' -> Record result'
        fillInImpl' = fillInImpl
          (R.RLProxy :: R.RLProxy rest)
          (R.RLProxy :: R.RLProxy dL')
        recurse :: Record result'
        recurse = fillInImpl' subpart $ Rec.delete key dfs

-- | If a partial option exists as a Maybe whose argument matches the type of
-- | the full option, insert this value or the default value into the result.
instance fillInConsMaybe ::
    -- ensure it exists in the result
  ( RowCons sym t defaults' defaults
    -- recurse through the remaining keys, subtracting from defaults
    -- and building a subresult
  , FillInImpl rest partial' dLJ defaults' result'
  , ShuffleNextToTop rest dL dLJ
  , FillInImpl rest partial' dLN defaults result
  , ShuffleNextToTop rest (R.Cons sym t dL) dLN
    -- at this key from partial to the result
  , RowCons sym t result' result
  , RowCons sym (Maybe t) partial' partial
  , IsSymbol sym
  , R.RowLacks sym partial'
  , R.RowLacks sym defaults'
  , R.RowLacks sym result'
  ) => FillInImpl (R.Cons sym (Maybe t) rest) partial (R.Cons sym t dL) defaults result
  where
    fillInImpl _ _ part dfs =
      case Rec.get key part of
        Nothing -> fillInImplN subpart dfs
        Just val ->
          let
            recurse :: Record result'
            recurse = fillInImplJ subpart $ Rec.delete key dfs
          in Rec.insert key val recurse
      where
        key = SProxy :: SProxy sym
        subpart :: Record partial'
        subpart = Rec.delete key (part :: Record partial)
        fillInImplJ :: Record partial' -> Record defaults' -> Record result'
        fillInImplJ = fillInImpl
          (R.RLProxy :: R.RLProxy rest)
          (R.RLProxy :: R.RLProxy dLJ)
        fillInImplN :: Record partial' -> Record defaults -> Record result
        fillInImplN = fillInImpl
          (R.RLProxy :: R.RLProxy rest)
          (R.RLProxy :: R.RLProxy dLN)

-- | An enumeration of all the options that may be passed to function-plot.
-- | Parameterized over xAxis and yAxis records to allow them to be
-- | under-specified.
type MaxOptions x y =
  ( target :: QuerySelector
  , title :: String
  , xAxis :: x
  , yAxis :: y
  , disableZoom :: Disj Boolean
  , grid :: Disj Boolean
  , tip ::
      { xLine :: Disj Boolean
      , yLine :: Disj Boolean
      , renderer :: Fn3 Number Number Int String
      }
  , annotations :: Array
      { x :: Number
      , y :: Number
      , text :: String
      }
  , data :: Data
  )
-- | The potential options for `options.xAxis` and `options.yAxis`.
type MaxAxisOptions =
  ( type :: AxisType
  , domain :: Interval
  , invert :: XDisj
  , label :: String
  )
-- | Constrained option type.
type Options o =
  forall o' x x' y y'.
    Union x x' MaxAxisOptions =>
    Union y y' MaxAxisOptions =>
    Union o o' (MaxOptions (Record x) (Record y)) =>
  Record o
-- | **All** the options.
type ReallyMaxOptions = Record (MaxOptions (Record MaxAxisOptions) (Record MaxAxisOptions))

-- | Defaults for **all** the options. (Mostly `mempty`s.)
defaultOptions :: ReallyMaxOptions
defaultOptions =
  { target: QuerySelector mempty
  , title: mempty
  , xAxis: defaultAxisOptions
  , yAxis: defaultAxisOptions
  , disableZoom: mempty
  , grid: mempty
  , tip:
    { xLine: mempty
    , yLine: mempty
    -- default tooltip renderer:
    -- https://github.com/mauriciopoppe/function-plot/blob/master/lib/tip.js#L16-L18
    , renderer: mkFn3 \x y i -> mempty
      --let showCoord =
      --in mkFn3 \x y i ->
    }
  , annotations: mempty
  , data: []
  }
-- | Default values for a (linear) axis.
defaultAxisOptions :: Record MaxAxisOptions
defaultAxisOptions =
  { type: LinearAxis
  , domain: mempty -- Default :: Interval
  , invert: mempty -- false, XORed
  , label: mempty -- ""
  }

sineNomine :: ReallyMaxOptions
sineNomine =
  fillIn defaultOptions { title: "No Name" }

maybeTitle :: Maybe String -> ReallyMaxOptions
maybeTitle title =
  unsafeFillIn { title }

-- | Value for `options.{x,y}Axis.type`: "linear" (default) or "log"
data AxisType = LinearAxis | LogAxis

-- | Boolean Monoid with XOR, since `invert <<< invert == id`.
newtype XDisj = XDisj Boolean
derive instance newtypeXDisj :: Newtype XDisj _

instance semigroupXDisj :: Semigroup XDisj where
  append (XDisj false) = id
  append (XDisj true) = over XDisj not

instance monoidXDisj :: Monoid XDisj where
  mempty = XDisj false

-- | An `Array` of `Datum`s, a.k.a. pretty things to graph. See below.
type Data = Array Datum

-- | Existential foreign type for a `Datum`.
foreign import data Datum :: Type

-- | `DatumU` appropriately quantified and constrained.
type DatumQuantified =
  forall datum datum' attr.
    Union datum datum' (MaxDataOptions attr) =>
  DatumU datum

-- | Convert a PureScript-built `Datum` to the `Foreign` value.
-- |
-- | TODO: `Foreign` encode?
mkDatum :: DatumQuantified -> Datum
mkDatum = unsafeCoerce

-- | Product of general datum options and the variant over the type of graph.
data DatumU datum = Datum (Record datum) DatumV

-- | All the options shared by all graph types
type MaxDataOptions attr =
  ( title :: String
  , skipTip :: Disj Boolean
  , range :: Interval
  , nSamples :: Int -- listed as a "number" in JS docs, but should be an int
  , graphType :: GraphType -- includes sampler
  , color :: Color
  , attr :: Record attr
  , derivative ::
      WithUpdatePolicy "x0"
      ( fn :: FunctionValue "x" )
  , secants :: Array
      (WithUpdatePolicy "x1"
      ( x0 :: Number ))
  )

-- | Variant of data over each graph type.
type DatumV = Variant
  ( linear :: { fn :: FunctionValue "x" }
  , parametric ::
    { x :: FunctionValue "t"
    , y :: FunctionValue "t"
    }
  , polar :: { r :: FunctionValue "theta" }
  , implicit :: { fn :: FunctionValue2 "x" "y" }
  , points :: { points :: Points }
  , vector :: { vector :: Point, offset :: Maybe Point }
  )

-- | A function that can be plotted on a graph. Arguments in phantom `Symbol`.
data FunctionValue (s :: Symbol)
  = StrFn String
  | FnVFn (Number -> Number)

data FunctionValue2 (a :: Symbol) (b :: Symbol)
  = StrFn2 String
  | FnVFn2 (Fn2 Number Number Number)

-- | `Array` of `Point`s.
type Points = Array Point
-- | A `Point` on the graph, (x, y)
data Point = Point Number Number

-- | Type of the graph, "interval", "polyline", or "scatter", including a
-- | `Sampler` and `Closed` options for the latter two types (i.e. not
-- | "interval" -- this ADT ensures this is hard to mess up).
data GraphType
  = IntervalGraph
  | PolylineGraph Sampler Closed
  | ScatterGraph Sampler Closed
-- | Use interval arithmetic or normal.
data Sampler = IntervalSampler | BuiltInSampler
-- | Closed means draw the line from the x-axis to the point on the curve.
data Closed = Closed | Open

-- | An interval. `Monoid` chooses to expand the interval.
data Interval = Interval (Min Number) (Max Number) | Default

instance semigroupInterval :: Semigroup Interval where
  append Default r = r
  append l Default = l
  append (Interval l1 r1) (Interval l2 r2) =
    Interval (l1 <> l2) (r1 <> r2)

instance monoidInterval :: Monoid Interval where
  mempty = Default

-- | Just a weird type synonym for allowing an update policy on a specific key.
type WithUpdatePolicy (sym :: Symbol) row =
  forall with.
    RowCons sym Number row with =>
  SplitUpdatePolicy (Record with) (Record row)

-- | An update policy representing when to update `x0` on a `derivative` or
-- | `x1` on a `secant`. The JS equivalent is confusing, but it amounts to:
-- |   - `At`: set a constant point, do not follow the mouse
-- |   - `StartAt`: follow the mouse, but start at a specific point
-- |   - `Updating`: follow the mouse once available
data SplitUpdatePolicy with without
  = At with
  | StartAt with
  | Updating without
