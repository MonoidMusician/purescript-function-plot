module FunctionPlot.Types.Internal where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Record as Rec
import Type.Prelude (class IsSymbol, SProxy(..))
import Type.Row as R
import Unsafe.Coerce (unsafeCoerce)

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

-- | An unsafe version of fillIn that leaves the missing fields blank.
unsafeFillIn :: forall p f. FillIn p f => p -> f
unsafeFillIn = fillIn (unsafeCoerce {})

instance fillInInstance ::
    -- partial :: Row <-> parts :: RowList
  ( R.RowToList partial parts
  , R.ListToRow parts partial
    -- filledIn :: Row <-> defaultL :: RowList
  , R.RowToList filledIn defaultL
  , R.ListToRow defaultL filledIn
    -- ensure we can instance match against the first row of parts
  , ShuffleNextToTop parts defaultL dL
    -- dispatch to the RowList implementation of this
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
  ) => ShuffleNextToTop
    -- get the top
    (R.Cons sym t1 rest)
    -- get the remaining rows
    i
    -- and add back on top
    (R.Cons sym t2 o)

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
