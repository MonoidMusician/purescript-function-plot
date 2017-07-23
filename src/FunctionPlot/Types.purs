module FunctionPlot.Types where

import Prelude

import Color (Color)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Function.Uncurried (Fn3, mkFn3)
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

class FillIn
  (partial :: Type) -- Record
  (filledIn :: Type) -- Record
  -- | partial -> filledIn
  where
    fillIn :: filledIn -> partial -> filledIn

instance fillInInstance ::
  ( R.RowToList partial parts
  , R.ListToRow parts partial
  , FillInImpl parts partial filledIn filledIn
  ) => FillIn (Record partial) (Record filledIn)
  where
    fillIn dfs part = fillInImpl (R.RLProxy :: R.RLProxy parts) part dfs

class FillInImpl
  (iter :: R.RowList)
  (partial :: # Type)
  (defaults :: # Type)
  (result :: # Type)
  | iter defaults -> partial result
  , iter -> partial
  where
    fillInImpl ::
      --forall partial.
        --R.ListToRow iter partial =>
      R.RLProxy iter -> Record partial -> Record defaults -> Record result

instance fillInNil ::
  FillInImpl R.Nil () remaining remaining where
    fillInImpl _ _{-{}-} dfs = dfs

instance fillInConsZExists ::
    -- ensure it exists in the result
  ( RowCons sym t defaults' defaults
    -- recurse through the remaining keys, subtracting from defaults
    -- and building a subresult
  , FillInImpl rest partial' defaults' result'
    -- at this key from partial to the result
  , RowCons sym t result' result
  , RowCons sym t partial' partial
  , IsSymbol sym
  , R.RowLacks sym partial'
  , R.RowLacks sym defaults'
  , R.RowLacks sym result'
  ) => FillInImpl (R.Cons sym t rest) partial defaults result
  where
    fillInImpl _ part dfs = Rec.insert key val recurse
      where
        key = SProxy :: SProxy sym
        val :: t
        val = Rec.get key part
        subpart :: Record partial'
        subpart = Rec.delete key (part :: Record partial)
        fillInImpl' :: Record partial' -> Record defaults' -> Record result'
        fillInImpl' = fillInImpl (R.RLProxy :: R.RLProxy rest)
        recurse :: Record result'
        recurse = fillInImpl' subpart $ Rec.delete key dfs

instance fillInConsMaybe ::
    -- ensure it exists in the result
  ( RowCons sym t defaults' defaults
    -- recurse through the remaining keys, subtracting from defaults
    -- and building a subresult
  , FillInImpl rest partial' defaults' result'
  , FillInImpl rest partial' defaults result
    -- at this key from partial to the result
  , RowCons sym t result' result
  , RowCons sym (Maybe t) partial' partial
  , IsSymbol sym
  , R.RowLacks sym partial'
  , R.RowLacks sym defaults'
  , R.RowLacks sym result'
  ) => FillInImpl (R.Cons sym (Maybe t) rest) partial defaults result
  where
    fillInImpl _ part dfs =
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
        fillInImplJ = fillInImpl (R.RLProxy :: R.RLProxy rest)
        fillInImplN :: Record partial' -> Record defaults -> Record result
        fillInImplN = fillInImpl (R.RLProxy :: R.RLProxy rest)

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
type MaxAxisOptions =
  ( type :: AxisType
  , domain :: Interval
  , invert :: XDisj
  , label :: String
  )
type Options o =
  forall o' x x' y y'.
    Union x x' MaxAxisOptions =>
    Union y y' MaxAxisOptions =>
    Union o o' (MaxOptions (Record x) (Record y)) =>
  Record o
type ReallyMaxOptions = Record (MaxOptions (Record MaxAxisOptions) (Record MaxAxisOptions))

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
defaultAxisOptions :: Record MaxAxisOptions
defaultAxisOptions =
  { type: LinearAxis
  , domain: mempty
  , invert: mempty
  , label: mempty
  }

sineNomine :: ReallyMaxOptions
sineNomine =
  fillIn defaultOptions { title: "No Name" }

maybeTitle :: Maybe String -> ReallyMaxOptions
maybeTitle title =
  unsafeFillIn { title }

data AxisType = LinearAxis | LogAxis

newtype XDisj = XDisj Boolean
derive instance newtypeXDisj :: Newtype XDisj _

instance semigroupXDisj :: Semigroup XDisj where
  append (XDisj false) = id
  append (XDisj true) = over XDisj not

instance monoidXDisj :: Monoid XDisj where
  mempty = XDisj false

type Data = Array Datum

foreign import data Datum :: Type

type DatumQuantified =
  forall datum datum' attr.
    Union datum datum' (MaxDataOptions attr) =>
  DatumU datum

mkDatum :: DatumQuantified -> Datum
mkDatum = unsafeCoerce

data DatumU datum = Datum (Record datum) DatumV

type MaxDataOptions attr =
  ( title :: String
  , skipTip :: Boolean
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

type DatumV = Variant
  ( linear :: { fn :: FunctionValue "x" }
  , parametric ::
    { x :: FunctionValue "t"
    , y :: FunctionValue "t"
    }
  , polar :: { r :: FunctionValue "theta" }
  , implicit :: { fn :: FunctionValue "x,y" }
  , points :: { points :: Points }
  , vector :: { vector :: Point, offset :: Maybe Point }
  )

data FunctionValue (s :: Symbol)
  = StrFn String
  | FnFn (Number -> Number)

type Points = Array Point
data Point = Point Number Number

data GraphType
  = IntervalGraph
  | PolylineGraph Sampler Closed
  | ScatterGraph Sampler Closed
data Sampler = IntervalSampler | BuiltInSampler
data Closed = Closed | Open

data Interval = Interval (Min Number) (Max Number) | Default

instance semigroupInterval :: Semigroup Interval where
  append Default r = r
  append l Default = l
  append (Interval l1 r1) (Interval l2 r2) =
    Interval (l1 <> l2) (r1 <> r2)

instance monoidInterval :: Monoid Interval where
  mempty = Default

type WithUpdatePolicy (sym :: Symbol) row =
  forall with.
    RowCons sym Number row with =>
  SplitUpdatePolicy (Record with) (Record row)

data SplitUpdatePolicy with without
  = At with
  | StartAt with
  | Updating without
