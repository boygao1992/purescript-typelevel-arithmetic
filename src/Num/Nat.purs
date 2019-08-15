module Num.Nat where

import Prelude ((*), (+))

import Num.Digit as Digit
import Symbol.Utils (class ReverseSymbol)
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Bool
import Type.Data.Ordering as Ord
import Type.Data.Ordering (OProxy(..))
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol

-- | Type-level Natural Number
-- NOTE operators don't guarantee IsNat constraint

-- | IsNat

class IsNat (n :: Symbol) where
  reifyNat :: SProxy n -> Int

instance isNatImpl ::
  ( NormalizeRemoveZero n n0
  , ReverseSymbol n0 n1
  , IsNatReversed n1
  ) => IsNat n where
    reifyNat _ = reifyNatReversed (SProxy :: SProxy n1)

class IsNatReversed (n :: Symbol) where
  reifyNatReversed :: SProxy n -> Int

instance isNatReversedImpl
  :: ( Symbol.Cons h t n
    , IsNatCons h t
    )
  => IsNatReversed n where
    reifyNatReversed _ = reifyNatCons (SProxy :: SProxy h) (SProxy :: SProxy t)

class IsNatCons (h :: Symbol) (t :: Symbol) where
  reifyNatCons :: SProxy h -> SProxy t -> Int

instance isNatImplBaseCase ::
  ( Digit.IsDigit h
  ) => IsNatCons h "" where
    reifyNatCons _ _ = Digit.reifyDigit (SProxy :: SProxy h)
else
instance isNatImplInductionStep ::
  ( Digit.IsDigit h
  , IsNatReversed t
  ) => IsNatCons h t where
    reifyNatCons _ _
      = Digit.reifyDigit (SProxy :: SProxy h)
      + reifyNatReversed (SProxy :: SProxy t) * 10

-- | IsNatPred

class IsNatPred (n :: Symbol) (b :: Bool.Boolean) | n -> b

instance isNatPredEmpty :: IsNatPred "" Bool.False
else
instance isNatPredOtherwise ::
  ( Symbol.Cons h t n
  , IsNatPredImpl h t b
  ) => IsNatPred n b

class IsNatPredImpl (h :: Symbol) (t :: Symbol) (b :: Bool.Boolean) | h t -> b

instance isNatImplPredBaseCase ::
  ( Digit.IsDigitPred h b
  ) => IsNatPredImpl h "" b
else
instance isNatImplPredInductionStep ::
  ( Digit.IsDigitPred h b1
  , IsNatPred t b2
  , Bool.And b1 b2 b
  ) => IsNatPredImpl h t b

isNatPred :: forall n b. IsNatPred n b => SProxy n -> BProxy b
isNatPred _ = BProxy :: BProxy b

-- | Type-level Arithmetic: Add

class Add (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance addAll ::
  ( ReverseSymbol x x'
  , ReverseSymbol y y'
  , AddReversed x' y' "0" z'
  , ReverseSymbol z' z
  ) => Add x y z

class AddReversed (x :: Symbol) (y :: Symbol) (carry :: Symbol) (z :: Symbol) | x y carry -> z

instance addReversedBaseCase1 :: AddReversed "" y "0" y
else
instance addReversedBaseCase2 :: AddReversed x "" "0" x
else
instance addReversedBaseCase3 ::
  ( AddReversed "1" y "0" z
  ) => AddReversed "" y "1" z
else
instance addReversedBaseCase4 ::
  ( AddReversed x "1" "0" z
  ) => AddReversed x "" "1" z
else
instance addReversedInductionStep ::
  ( Symbol.Cons x_h x_t x
  , Symbol.Cons y_h y_t y
  , AddReversedInductionStep x_h x_t y_h y_t carry z
  ) => AddReversed x y carry z

class AddReversedInductionStep (x_h :: Symbol) (x_t :: Symbol) (y_h :: Symbol) (y_t :: Symbol) (carry :: Symbol) (z :: Symbol) | x_h x_t y_h y_t carry -> z

instance addReversedInductionStepImpl ::
  ( Digit.Add x_h y_h carry1 z1
  , Digit.Add z1 carry0 carry2 z2
  , Digit.Add carry1 carry2 "0" carry3
  , AddReversed x_t y_t carry3 z_rest
  , Symbol.Append z2 z_rest z
  ) => AddReversedInductionStep x_h x_t y_h y_t carry0 z

add :: forall x y z. Add x y z => SProxy x -> SProxy y -> SProxy z
add _ _ = SProxy :: SProxy z

-- | Type-level Arithmetic: Successor

class Succ (pred :: Symbol) (succ :: Symbol) | pred -> succ

instance succImpl ::
  ( IsNat pred
  , Add pred "1" succ
  ) => Succ pred succ

succ :: forall pred succ. Succ pred succ => SProxy pred -> SProxy succ
succ _ = SProxy :: SProxy succ

-- succExample1 :: SProxy "1112"
-- succExample1 = succ $ SProxy :: SProxy "1111"

-- succExample2 :: SProxy "1"
-- succExample2 = succ $ SProxy :: SProxy "0"

-- | Normalize

class NormalizeRemoveZero (num :: Symbol) (normalized :: Symbol) | num -> normalized
instance normalizeRemoveZeroImpl ::
  ( NormalizeRemoveZeroBaseCase num o
  , NormalizeRemoveZeroRecoverZero o normalized
  ) => NormalizeRemoveZero num normalized

class NormalizeRemoveZeroBaseCase (num :: Symbol) (o :: Symbol) | num -> o
instance normalizeRemoveZeroBaseCaseEmpty ::
  NormalizeRemoveZeroBaseCase "" ""
else instance normalizeRemoveZeroBaseCaseNonEmpty ::
  ( Symbol.Cons h t num
  , NormalizeRemoveZeroInductionStep h t o
  ) => NormalizeRemoveZeroBaseCase num o

class NormalizeRemoveZeroInductionStep (h :: Symbol) (t :: Symbol) (o :: Symbol) | h t -> o

instance normalizeRemoveZeroInductionStepZero ::
  ( NormalizeRemoveZeroBaseCase t o
  ) => NormalizeRemoveZeroInductionStep "0" t o
else instance normalizeRemoveZeroInductionStepNonZero ::
  ( Symbol.Append h t o
  ) => NormalizeRemoveZeroInductionStep h t o

class NormalizeRemoveZeroRecoverZero (i :: Symbol) (o :: Symbol) | i -> o

instance normalizeRemoveZeroRecoverZeroEmpty ::
  NormalizeRemoveZeroRecoverZero "" "0"
else instance normalizeRemoveZeroRecoverZeroNonEmpty ::
  NormalizeRemoveZeroRecoverZero i i

-- | Compare

class Compare (x :: Symbol) (y :: Symbol) (o :: Ord.Ordering) | x y -> o

instance compareNonNegativeImpl ::
  ( ReverseSymbol x x'
  , ReverseSymbol y y'
  , CompareReversed x' y' o
  ) => Compare x y o

class CompareReversed (x :: Symbol) (y :: Symbol) (o :: Ord.Ordering) | x y -> o

instance compareNonNegativeReversedBaseCase1 ::
  CompareReversed "" "" Ord.EQ
else
instance compareNonNegativeReversedBaseCase2 ::
  CompareReversed "" y Ord.LT
else
instance compareNonNegativeReversedBaseCase3 ::
  CompareReversed x "" Ord.GT
else
instance compareNonNegativeReversedInductionStep ::
  ( Symbol.Cons x_h x_t x
  , Symbol.Cons y_h y_t y
  , CompareReversed x_t y_t o1
  , Digit.Compare x_h y_h o2
  , Ord.Append o1 o2 o
  ) => CompareReversed x y o

compare :: forall a b o. Compare a b o => SProxy a -> SProxy b -> OProxy o
compare _ _ = OProxy :: OProxy o

-- | Eq

class Eq (x :: Symbol) (y :: Symbol)

instance eqNat :: Compare x y Ord.EQ => Eq x y
