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
  reflectNat :: SProxy n -> Int

instance isNatImpl ::
  ( NormalizeRemoveZero n n0
  , ReverseSymbol n0 n1
  , IsNatReversed n1
  ) => IsNat n where
    reflectNat _ = reflectNatReversed (SProxy :: SProxy n1)

class IsNatReversed (n :: Symbol) where
  reflectNatReversed :: SProxy n -> Int

instance isNatReversedImpl
  :: ( Symbol.Cons h t n
    , IsNatCons h t
    )
  => IsNatReversed n where
    reflectNatReversed _ = reflectNatCons (SProxy :: SProxy h) (SProxy :: SProxy t)

class IsNatCons (h :: Symbol) (t :: Symbol) where
  reflectNatCons :: SProxy h -> SProxy t -> Int

instance isNatImplBaseCase ::
  ( Digit.IsDigit h
  ) => IsNatCons h "" where
    reflectNatCons _ _ = Digit.reflectDigit (SProxy :: SProxy h)
else
instance isNatImplInductionStep ::
  ( Digit.IsDigit h
  , IsNatReversed t
  ) => IsNatCons h t where
    reflectNatCons _ _
      = Digit.reflectDigit (SProxy :: SProxy h)
      + reflectNatReversed (SProxy :: SProxy t) * 10

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
  ( Add pred "1" succ
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

-- | Length

class Length (s :: Symbol) (n :: Symbol) | s -> n

instance lengthZero :: Length "" "0"
else
instance lengthPositive ::
  ( Symbol.Cons s_h s_t s
  , Length s_t n'
  , Succ n' n
  )
  => Length s n

length :: forall s n. Length s n => SProxy s -> SProxy n
length _ = SProxy :: SProxy n

-- lengthExample0 :: SProxy "0"
-- lengthExample0 = length (SProxy :: SProxy "")
-- lengthExample1 :: SProxy "5"
-- lengthExample1 = length (SProxy :: SProxy "wenbo")

-- | DividedByDigit

class Digit.IsDigit y <=
  DivideByDigit (x :: Symbol) (y :: Symbol) (z :: Symbol) (remainder :: Symbol)
  | x y -> z remainder

instance divideByDigitInit ::
  ( Digit.IsDigit y
  , Symbol.Cons x_h x_t x
  , DivideByDigitImpl "0" x_h x_t y z' remainder
  , NormalizeRemoveZero z' z
  )
  => DivideByDigit x y z remainder

class Digit.IsDigit y <=
  DivideByDigitImpl (x0 :: Symbol) (x1 :: Symbol) (x_t :: Symbol) (y :: Symbol) (z :: Symbol) (remainder :: Symbol)
  | x0 x1 x_t y -> z remainder

instance divideByDigitBaseCase ::
  ( Digit.Divide x0 "0" y z1 r1
  , Digit.Divide "0" x1 y z2 r2
  , Digit.Add z1 z2 "0" z
  , Digit.Add r1 r2 "0" r
  )
  => DivideByDigitImpl x0 x1 "" y z r
else
instance divideByDigitInductionStep ::
  ( Digit.Divide x0 "0" y z_h1 r1
  , Digit.Divide "0" x1 y z_h2 r2
  , Digit.Add z_h1 z_h2 "0" z_h
  , Digit.Add r1 r2 "0" r
  , Symbol.Cons x_t_h x_t_t x_t
  , DivideByDigitImpl r x_t_h x_t_t y z_t remainder
  , Symbol.Append z_h z_t z
  )
  => DivideByDigitImpl x0 x1 x_t y z remainder

-- data Tuple a b = Tuple a b
-- divideByDigit :: forall x y z r. DivideByDigit x y z r => SProxy x -> SProxy y -> Tuple (SProxy z) (SProxy r)
-- divideByDigit _ _ = Tuple (SProxy :: SProxy z) (SProxy :: SProxy r)

-- divideByDigitExample1 :: Tuple (SProxy "61") (SProxy "1")
-- divideByDigitExample1 = divideByDigit (SProxy :: SProxy "123") (SProxy :: SProxy "2")

-- | Multiply
-- NOTE [Karatsuba algorithm](https://en.wikipedia.org/wiki/Karatsuba_algorithm)
-- Base = 10, m = half

class Multiply (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

