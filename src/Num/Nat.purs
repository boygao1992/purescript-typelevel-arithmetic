module Num.Nat where

import Prelude ((*), (+))

import Num.Digit as Digit
import Prim.TypeError (class Fail, Text)
import Symbol.Utils (class ReverseSymbol)
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Bool
import Type.Data.Ordering (OProxy(..))
import Type.Data.Ordering as Ord
import Type.Data.Symbol (SProxy(..))
import Type.Data.Symbol as Symbol
import Type.Utils as Type

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

-- | Add

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

-- | Successor

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

instance compareLeftNull ::
  Fail (Text "Compare: empty input received to the left")
  => Compare "" y o
else
instance compareRightNull ::
  Fail (Text "Compare: empty input received to the right")
  => Compare x "" o
else
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

-- | Max
class Max (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance maxImpl ::
  ( Compare x y ord
  , Type.IsEqualPred (OProxy ord) (OProxy Ord.GT) isGT
  , Bool.If isGT (SProxy x) (SProxy y) (SProxy z)
  )
  => Max x y z

max :: forall x y z. Max x y z => SProxy x -> SProxy y -> SProxy z
max _ _ = SProxy :: SProxy z

-- maxExample :: SProxy "2389"
-- maxExample = max (SProxy :: SProxy "1389") (SProxy :: SProxy "2389")

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

-- | DivideByDigit

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

-- | Minus
-- NOTE assumption: x >= y

class Minus (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance minusInit ::
  ( ReverseSymbol x x'
  , ReverseSymbol y y'
  , MinusReversed "0" x' y' z'
  , ReverseSymbol z' z0
  , NormalizeRemoveZero z0 z
  )
  => Minus x y z

class Digit.IsDigit carry <=
  MinusReversed (carry :: Symbol) (x :: Symbol) (y :: Symbol) (z :: Symbol) | carry x y -> z

-- both null
instance minusReversedBase0 ::
  MinusReversed "0" "" "" ""
-- x null
else
instance minusReversedError ::
  ( Fail (Text "Minus: x < y")
  , Digit.IsDigit carry
  )
  => MinusReversed carry "" y z -- carry > 0 || length y > 0
else
instance minusReversedBase1 ::
  MinusReversed "0" x "" x
-- y null
else
instance minusReversedBase2 ::
  ( Digit.IsDigit carry
  , MinusReversed "0" x carry z
  )
  => MinusReversed carry x "" z
-- both non-null
else
instance minusReversedCons ::
  ( Symbol.Cons x_h x_t x
  , Symbol.Cons y_h y_t y
  , MinusReversedCons carry x_h x_t y_h y_t z
  )
  => MinusReversed carry x y z

class (Digit.IsDigit carry, Digit.IsDigit x_h, Digit.IsDigit y_h) <=
  MinusReversedCons (carry :: Symbol) (x_h :: Symbol) (x_t :: Symbol) (y_h :: Symbol) (y_t :: Symbol) (z :: Symbol) | carry x_h x_t y_h y_t -> z

{-
e.g. 100 - 099
carry = 1
x = 0
y = 9
x - y -> carry' = 1, z' = 1

x = 0
y + carry -> y_carry = 1, y' = 0
x - y' -> carry' = 0, z = 0
y_carry + carry' -> carry = 1

e.g. 100 - 009
carry = 1
x = 0
y = 0
x - y -> carry' = 0, z' = 0

x = 0
y + carry -> y_carry = 0, y' = 1
x - y' -> carry' = 1, z_h = 9
y_carry + carry' -> carry = 1

-}

instance minusReversedConsImpl ::
  ( Digit.IsDigit carry
  , Digit.IsDigit x_h
  , Digit.IsDigit y_h
  , Digit.Add y_h carry y_carry y_h'
  , Digit.Minus x_h y_h' carry' z_h
  , Digit.Add y_carry carry' "0" carry1
  , MinusReversed carry1 x_t y_t z_t
  , Symbol.Append z_h z_t z
  )
  => MinusReversedCons carry x_h x_t y_h y_t z

minus :: forall x y z. Minus x y z => SProxy x -> SProxy y -> SProxy z
minus _ _ = SProxy :: SProxy z

-- minusExample1 :: SProxy "91"
-- minusExample1 = minus (SProxy :: SProxy "123") (SProxy :: SProxy "32")
-- minusExample2 :: SProxy "9999"
-- minusExample2 = minus (SProxy :: SProxy "10000") (SProxy :: SProxy "1")

-- | Pred

class Pred (i :: Symbol) (o :: Symbol) | i -> o

instance predBase ::
  Fail (Text "Pred of 0 is not defined")
  => Pred "0" o
else
instance predImpl ::
  ( Minus i "1" o
  )
  => Pred i o

pred :: forall i o. Pred i o => SProxy i -> SProxy o
pred _ = SProxy :: SProxy o

-- predExample1 :: SProxy "999999999"
-- predExample1 = pred (SProxy :: SProxy "1000000000")

-- | Partition

class Partition (s :: Symbol) (n :: Symbol) (l :: Symbol) (r :: Symbol)
  | s n -> l r

instance partitionImpl ::
  PartitionImpl "" s n l r
  => Partition s n l r

class PartitionImpl (acc :: Symbol) (rest :: Symbol) (n :: Symbol) (l :: Symbol) (r :: Symbol) | acc rest n -> l r

instance partitionBase0 :: PartitionImpl acc rest "0" acc rest
-- else
-- instance partitionError ::
--   Fail (Text "PartitionError: n is greater than input length")
--   => PartitionImpl acc "" n l r
else
instance partitionBase1 :: PartitionImpl acc "" n acc "" -- NOTE choice made for class Multiply
else
instance partitionInductionStep ::
  ( Symbol.Cons rest_h rest_t rest
  , Pred n n_1
  , Symbol.Append acc rest_h acc'
  , PartitionImpl acc' rest_t n_1 l r
  )
  => PartitionImpl acc rest n l r

-- data Tuple a b = Tuple a b
-- partition :: forall s n l r. Partition s n l r => SProxy s -> SProxy n -> Tuple (SProxy l) (SProxy r)
-- partition _ _ = Tuple (SProxy :: SProxy l) (SProxy :: SProxy r)

-- partitionExample1 :: Tuple (SProxy "123") (SProxy "456")
-- partitionExample1 = partition (SProxy :: SProxy "123456") (SProxy :: SProxy "3")

-- | Ceil

class Ceil (i :: Symbol) (remainder :: Symbol) (o :: Symbol) | i remainder -> o

instance ceilZero :: Ceil i "0" i
else
instance ceilOtherwise :: Succ i o => Ceil i r o

-- | Floor

class Floor (i :: Symbol) (remainder :: Symbol) (o :: Symbol) | i remainder -> o

instance floorImpl :: Floor i r i

-- | TODO Round

class Round (divisor :: Symbol) (i :: Symbol) (remainder :: Symbol) (o :: Symbol)
  | divisor i remainder -> o

-- | ShiftBase10

class ShiftBase10 (i :: Symbol) (n :: Symbol) (o :: Symbol) | i n -> o

instance shiftBase10BaseCase :: ShiftBase10 i "0" i
else
instance shiftBase10InductionStep ::
  ( Symbol.Append i "0" i'
  , Pred n n_1
  , ShiftBase10 i' n_1 o
  )
  => ShiftBase10 i n o

shiftBase10 :: forall i n o. ShiftBase10 i n o => SProxy i -> SProxy n -> SProxy o
shiftBase10 _ _ = SProxy

-- shiftBase10Example1 :: SProxy "123400"
-- shiftBase10Example1 = shiftBase10 (SProxy :: SProxy "1234") (SProxy :: SProxy "2")

-- | ShiftBase10Reversed

class ShiftBase10Reversed (i :: Symbol) (n :: Symbol) (o :: Symbol) | i n -> o

instance shiftBase10ReversedBaseCase :: ShiftBase10Reversed i "0" i
else
instance shiftBase10ReversedInductionStep ::
  ( Symbol.Append "0" i i'
  , Pred n n_1
  , ShiftBase10Reversed i' n_1 o
  )
  => ShiftBase10Reversed i n o

shiftBase10Reversed :: forall i n o. ShiftBase10Reversed i n o => SProxy i -> SProxy n -> SProxy o
shiftBase10Reversed _ _ = SProxy

-- shiftBase10ReversedExample1 :: SProxy "000006"
-- shiftBase10ReversedExample1 = shiftBase10Reversed (SProxy :: SProxy "06") (SProxy :: SProxy "4")


-- | MultiplyByDigit

class Digit.IsDigit y <=
  MultiplyByDigit (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance multiplyByDigitInit ::
  ( Digit.IsDigit y
  , ReverseSymbol x x'
  , ReverseSymbol y y'
  , MultiplyByDigitReversed "0" x' y' z'
  , ReverseSymbol z' z0
  , NormalizeRemoveZero z0 z
  )
  => MultiplyByDigit x y z

class (Digit.IsDigit carry, Digit.IsDigit y) <=
  MultiplyByDigitReversed (carry :: Symbol) (x :: Symbol) (y :: Symbol) (z :: Symbol)
  | carry x y -> z

instance multiplyByDigitReversedBase ::
  ( Digit.IsDigit carry
  , Digit.IsDigit y
  )
  => MultiplyByDigitReversed carry "" y carry
else
instance multiplyByDigitReversedCons ::
  ( Digit.IsDigit carry
  , Symbol.Cons x_h x_t x
  , MultiplyByDigitReversedCons carry x_h x_t y z
  )
  => MultiplyByDigitReversed carry x y z

class (Digit.IsDigit x_h, Digit.IsDigit y) <=
  MultiplyByDigitReversedCons (carry :: Symbol) (x_h :: Symbol) (x_t :: Symbol) (y :: Symbol) (z :: Symbol)
  | carry x_h x_t y -> z

instance multiplyByDigitReversedConsBase ::
  ( Digit.Multiply x_h y carry' z_h0
  , Digit.Add z_h0 carry z_carry z_h
  , Digit.Add carry' z_carry "0" carry0
  , MultiplyByDigitReversed carry0 x_t y z_t
  , Symbol.Append z_h z_t z
  )
  => MultiplyByDigitReversedCons carry x_h x_t y z

multiplyByDigit :: forall x y z. MultiplyByDigit x y z => SProxy x -> SProxy y -> SProxy z
multiplyByDigit _ _ = SProxy

-- mutiplyByDigitExample1 :: SProxy "492"
-- mutiplyByDigitExample1 = multiplyByDigit (SProxy :: SProxy "123") (SProxy :: SProxy "4")
-- mutiplyByDigitExample2 :: SProxy "105"
-- mutiplyByDigitExample2 = multiplyByDigit (SProxy :: SProxy "35") (SProxy :: SProxy "3")

-- | Multiply
-- NOTE [Karatsuba algorithm](https://en.wikipedia.org/wiki/Karatsuba_algorithm)
-- Base = 10, m = half

class Multiply (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

-- NOTE 1234 x 567 = 699678
-- x="1234"
-- y="567"
instance multiplyInit ::
  ( ReverseSymbol x x'
  -- x'="4321" <- ReverseSymbol x
  , ReverseSymbol y y'
  -- y'="765" <- ReverseSymbol y
  , MultiplyReversed x' y' z'
  , ReverseSymbol z' z0
  , NormalizeRemoveZero z0 z
  )
  => Multiply x y z

class MultiplyReversed (x :: Symbol) (y :: Symbol) (z :: Symbol) | x y -> z

instance multiplyReversedImpl ::
  ( Length x xl
    -- l1="4" <- Length x'
  , Length y yl
    -- l2="3" <- Length y'
  , MultiplyReversedImpl x xl y yl z
  )
  => MultiplyReversed x y z

class MultiplyReversedImpl (x :: Symbol) (xl :: Symbol) (y :: Symbol) (yl :: Symbol) (z :: Symbol) | x xl y yl -> z

instance multiplyReversedImplBase0 ::
  MultiplyReversedImpl x xl "" "0" "0"
else
instance multiplyReversedImplBase1 ::
  MultiplyReversedImpl "" "0" y yl "0"
else
instance multiplyReversedImplBase2 ::
  ( Digit.Multiply x y z_t z_h
  , Symbol.Append z_h z_t z
  )
  => MultiplyReversedImpl x "1" y "1" z
else
instance multiplyReversedImplInductionStep ::
  ( Max xl yl l
  -- l="4" <- Max l1 l2
  , DivideByDigit l "2" m' m_r
  -- m'="2", m_r="0" <- DivideByDigit l "2"
  , Ceil m' m_r m
  -- m="2" <- Ceil m' m_r
  , Partition x m x_r x_l
  -- x_r="43", x_l="21"<- Partition x' m
  , Partition y m y_r y_l
  -- y_r="76", y_l="5" <- Partition y' m
  , MultiplyReversed x_l y_l z_ll
  -- z_ll="06" <- MultiplyReversed x_l y_l
  , MultiplyReversed x_l y_r z_lr
  -- z_lr="408" <- MultiplyReversed x_l y_r
  , MultiplyReversed x_r y_l z_rl
  -- z_rl="071" <- MultiplyReversed x_r y_l
  , MultiplyReversed x_r y_r z_rr
  -- z_rr="8722" <- MultiplyReversed x_r y_r
  , ShiftBase10Reversed z_ll m z_ll_m1
  -- z_ll_m1="0006" <- ShiftBase10Reversed z_ll m
  , AddReversed z_lr z_rl "0" z_m1'
  -- z_m1'="479" <- AddReversed z_lr z_rl
  , AddReversed z_m1' z_ll_m1 "0" z_m1
  -- z_m1="4796" <- AddReversed z_m1' z_ll_m1
  , ShiftBase10Reversed z_m1 m z_m0'
  -- z_m0'="004796" <- ShiftBase10Reversed z_m1 m
  , AddReversed z_m0' z_rr "0" z
  -- z_m0="876996" <- AddReversed z_m0' z_rr
  )
  => MultiplyReversedImpl x xl y yl z

multiply :: forall x y z. Multiply x y z => SProxy x -> SProxy y -> SProxy z
multiply _ _ = SProxy

-- multiplyExample1 :: SProxy "699678"
-- multiplyExample1 = multiply (SProxy :: SProxy "1234") (SProxy :: SProxy "567")
-- multiplyExample2 :: SProxy "68833502225"
-- multiplyExample2 = multiply (SProxy :: SProxy "53") (SProxy :: SProxy "1298745325")
