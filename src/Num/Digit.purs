module Num.Digit where

import Type.Data.Symbol (SProxy)
import Type.Data.Boolean (BProxy(..))
import Type.Data.Boolean as Bool
import Type.Data.Ordering as Ord
import Prim.TypeError

-- | Type-level Digit

class IsDigit (d :: Symbol) where
  reflectDigit :: SProxy d -> Int

instance numberZero  :: IsDigit "0" where reflectDigit _ = 0
instance numberOne   :: IsDigit "1" where reflectDigit _ = 1
instance numberTwo   :: IsDigit "2" where reflectDigit _ = 2
instance numberThree :: IsDigit "3" where reflectDigit _ = 3
instance numberFour  :: IsDigit "4" where reflectDigit _ = 4
instance numberFive  :: IsDigit "5" where reflectDigit _ = 5
instance numberSix   :: IsDigit "6" where reflectDigit _ = 6
instance numberSeven :: IsDigit "7" where reflectDigit _ = 7
instance numberEight :: IsDigit "8" where reflectDigit _ = 8
instance numberNine  :: IsDigit "9" where reflectDigit _ = 9

class IsDigitPred (d :: Symbol) (b :: Bool.Boolean) | d -> b

instance isDigitZero :: IsDigitPred "0" Bool.True
else instance isDigitOne :: IsDigitPred "1" Bool.True
else instance isDigitTwo :: IsDigitPred "2" Bool.True
else instance isDigitThree :: IsDigitPred "3" Bool.True
else instance isDigitFour :: IsDigitPred "4" Bool.True
else instance isDigitFive :: IsDigitPred "5" Bool.True
else instance isDigitSix :: IsDigitPred "6" Bool.True
else instance isDigitSeven :: IsDigitPred "7" Bool.True
else instance isDigitEight :: IsDigitPred "8" Bool.True
else instance isDigitNine :: IsDigitPred "9" Bool.True
else instance notDigit :: IsDigitPred d Bool.False

isDigitPred :: forall d b. IsDigitPred d b => SProxy d -> BProxy b
isDigitPred _ = BProxy :: BProxy b

-- | Compare
class (IsDigit x, IsDigit y) <=
  Compare (x :: Symbol) (y :: Symbol) (o :: Ord.Ordering) | x y -> o

instance compare00 :: Compare "0" "0" Ord.EQ
instance compare01 :: Compare "0" "1" Ord.LT
instance compare02 :: Compare "0" "2" Ord.LT
instance compare03 :: Compare "0" "3" Ord.LT
instance compare04 :: Compare "0" "4" Ord.LT
instance compare05 :: Compare "0" "5" Ord.LT
instance compare06 :: Compare "0" "6" Ord.LT
instance compare07 :: Compare "0" "7" Ord.LT
instance compare08 :: Compare "0" "8" Ord.LT
instance compare09 :: Compare "0" "9" Ord.LT
instance compare10 :: Compare "1" "0" Ord.GT
instance compare11 :: Compare "1" "1" Ord.EQ
instance compare12 :: Compare "1" "2" Ord.LT
instance compare13 :: Compare "1" "3" Ord.LT
instance compare14 :: Compare "1" "4" Ord.LT
instance compare15 :: Compare "1" "5" Ord.LT
instance compare16 :: Compare "1" "6" Ord.LT
instance compare17 :: Compare "1" "7" Ord.LT
instance compare18 :: Compare "1" "8" Ord.LT
instance compare19 :: Compare "1" "9" Ord.LT
instance compare20 :: Compare "2" "0" Ord.GT
instance compare21 :: Compare "2" "1" Ord.GT
instance compare22 :: Compare "2" "2" Ord.EQ
instance compare23 :: Compare "2" "3" Ord.LT
instance compare24 :: Compare "2" "4" Ord.LT
instance compare25 :: Compare "2" "5" Ord.LT
instance compare26 :: Compare "2" "6" Ord.LT
instance compare27 :: Compare "2" "7" Ord.LT
instance compare28 :: Compare "2" "8" Ord.LT
instance compare29 :: Compare "2" "9" Ord.LT
instance compare30 :: Compare "3" "0" Ord.GT
instance compare31 :: Compare "3" "1" Ord.GT
instance compare32 :: Compare "3" "2" Ord.GT
instance compare33 :: Compare "3" "3" Ord.EQ
instance compare34 :: Compare "3" "4" Ord.LT
instance compare35 :: Compare "3" "5" Ord.LT
instance compare36 :: Compare "3" "6" Ord.LT
instance compare37 :: Compare "3" "7" Ord.LT
instance compare38 :: Compare "3" "8" Ord.LT
instance compare39 :: Compare "3" "9" Ord.LT
instance compare40 :: Compare "4" "0" Ord.GT
instance compare41 :: Compare "4" "1" Ord.GT
instance compare42 :: Compare "4" "2" Ord.GT
instance compare43 :: Compare "4" "3" Ord.GT
instance compare44 :: Compare "4" "4" Ord.EQ
instance compare45 :: Compare "4" "5" Ord.LT
instance compare46 :: Compare "4" "6" Ord.LT
instance compare47 :: Compare "4" "7" Ord.LT
instance compare48 :: Compare "4" "8" Ord.LT
instance compare49 :: Compare "4" "9" Ord.LT
instance compare50 :: Compare "5" "0" Ord.GT
instance compare51 :: Compare "5" "1" Ord.GT
instance compare52 :: Compare "5" "2" Ord.GT
instance compare53 :: Compare "5" "3" Ord.GT
instance compare54 :: Compare "5" "4" Ord.GT
instance compare55 :: Compare "5" "5" Ord.EQ
instance compare56 :: Compare "5" "6" Ord.LT
instance compare57 :: Compare "5" "7" Ord.LT
instance compare58 :: Compare "5" "8" Ord.LT
instance compare59 :: Compare "5" "9" Ord.LT
instance compare60 :: Compare "6" "0" Ord.GT
instance compare61 :: Compare "6" "1" Ord.GT
instance compare62 :: Compare "6" "2" Ord.GT
instance compare63 :: Compare "6" "3" Ord.GT
instance compare64 :: Compare "6" "4" Ord.GT
instance compare65 :: Compare "6" "5" Ord.GT
instance compare66 :: Compare "6" "6" Ord.EQ
instance compare67 :: Compare "6" "7" Ord.LT
instance compare68 :: Compare "6" "8" Ord.LT
instance compare69 :: Compare "6" "9" Ord.LT
instance compare70 :: Compare "7" "0" Ord.GT
instance compare71 :: Compare "7" "1" Ord.GT
instance compare72 :: Compare "7" "2" Ord.GT
instance compare73 :: Compare "7" "3" Ord.GT
instance compare74 :: Compare "7" "4" Ord.GT
instance compare75 :: Compare "7" "5" Ord.GT
instance compare76 :: Compare "7" "6" Ord.GT
instance compare77 :: Compare "7" "7" Ord.EQ
instance compare78 :: Compare "7" "8" Ord.LT
instance compare79 :: Compare "7" "9" Ord.LT
instance compare80 :: Compare "8" "0" Ord.GT
instance compare81 :: Compare "8" "1" Ord.GT
instance compare82 :: Compare "8" "2" Ord.GT
instance compare83 :: Compare "8" "3" Ord.GT
instance compare84 :: Compare "8" "4" Ord.GT
instance compare85 :: Compare "8" "5" Ord.GT
instance compare86 :: Compare "8" "6" Ord.GT
instance compare87 :: Compare "8" "7" Ord.GT
instance compare88 :: Compare "8" "8" Ord.EQ
instance compare89 :: Compare "8" "9" Ord.LT
instance compare90 :: Compare "9" "0" Ord.GT
instance compare91 :: Compare "9" "1" Ord.GT
instance compare92 :: Compare "9" "2" Ord.GT
instance compare93 :: Compare "9" "3" Ord.GT
instance compare94 :: Compare "9" "4" Ord.GT
instance compare95 :: Compare "9" "5" Ord.GT
instance compare96 :: Compare "9" "6" Ord.GT
instance compare97 :: Compare "9" "7" Ord.GT
instance compare98 :: Compare "9" "8" Ord.GT
instance compare99 :: Compare "9" "9" Ord.EQ



-- | Add

class (IsDigit x, IsDigit y, IsDigit carry, IsDigit z) <=
  Add (x :: Symbol) (y :: Symbol) (carry :: Symbol) (z :: Symbol) | x y -> carry z

instance add00 :: Add "0" "0" "0" "0"
instance add01 :: Add "0" "1" "0" "1"
instance add02 :: Add "0" "2" "0" "2"
instance add03 :: Add "0" "3" "0" "3"
instance add04 :: Add "0" "4" "0" "4"
instance add05 :: Add "0" "5" "0" "5"
instance add06 :: Add "0" "6" "0" "6"
instance add07 :: Add "0" "7" "0" "7"
instance add08 :: Add "0" "8" "0" "8"
instance add09 :: Add "0" "9" "0" "9"
instance add10 :: Add "1" "0" "0" "1"
instance add11 :: Add "1" "1" "0" "2"
instance add12 :: Add "1" "2" "0" "3"
instance add13 :: Add "1" "3" "0" "4"
instance add14 :: Add "1" "4" "0" "5"
instance add15 :: Add "1" "5" "0" "6"
instance add16 :: Add "1" "6" "0" "7"
instance add17 :: Add "1" "7" "0" "8"
instance add18 :: Add "1" "8" "0" "9"
instance add19 :: Add "1" "9" "1" "0"
instance add20 :: Add "2" "0" "0" "2"
instance add21 :: Add "2" "1" "0" "3"
instance add22 :: Add "2" "2" "0" "4"
instance add23 :: Add "2" "3" "0" "5"
instance add24 :: Add "2" "4" "0" "6"
instance add25 :: Add "2" "5" "0" "7"
instance add26 :: Add "2" "6" "0" "8"
instance add27 :: Add "2" "7" "0" "9"
instance add28 :: Add "2" "8" "1" "0"
instance add29 :: Add "2" "9" "1" "1"
instance add30 :: Add "3" "0" "0" "3"
instance add31 :: Add "3" "1" "0" "4"
instance add32 :: Add "3" "2" "0" "5"
instance add33 :: Add "3" "3" "0" "6"
instance add34 :: Add "3" "4" "0" "7"
instance add35 :: Add "3" "5" "0" "8"
instance add36 :: Add "3" "6" "0" "9"
instance add37 :: Add "3" "7" "1" "0"
instance add38 :: Add "3" "8" "1" "1"
instance add39 :: Add "3" "9" "1" "2"
instance add40 :: Add "4" "0" "0" "4"
instance add41 :: Add "4" "1" "0" "5"
instance add42 :: Add "4" "2" "0" "6"
instance add43 :: Add "4" "3" "0" "7"
instance add44 :: Add "4" "4" "0" "8"
instance add45 :: Add "4" "5" "0" "9"
instance add46 :: Add "4" "6" "1" "0"
instance add47 :: Add "4" "7" "1" "1"
instance add48 :: Add "4" "8" "1" "2"
instance add49 :: Add "4" "9" "1" "3"
instance add50 :: Add "5" "0" "0" "5"
instance add51 :: Add "5" "1" "0" "6"
instance add52 :: Add "5" "2" "0" "7"
instance add53 :: Add "5" "3" "0" "8"
instance add54 :: Add "5" "4" "0" "9"
instance add55 :: Add "5" "5" "1" "0"
instance add56 :: Add "5" "6" "1" "1"
instance add57 :: Add "5" "7" "1" "2"
instance add58 :: Add "5" "8" "1" "3"
instance add59 :: Add "5" "9" "1" "4"
instance add60 :: Add "6" "0" "0" "6"
instance add61 :: Add "6" "1" "0" "7"
instance add62 :: Add "6" "2" "0" "8"
instance add63 :: Add "6" "3" "0" "9"
instance add64 :: Add "6" "4" "1" "0"
instance add65 :: Add "6" "5" "1" "1"
instance add66 :: Add "6" "6" "1" "2"
instance add67 :: Add "6" "7" "1" "3"
instance add68 :: Add "6" "8" "1" "4"
instance add69 :: Add "6" "9" "1" "5"
instance add70 :: Add "7" "0" "0" "7"
instance add71 :: Add "7" "1" "0" "8"
instance add72 :: Add "7" "2" "0" "9"
instance add73 :: Add "7" "3" "1" "0"
instance add74 :: Add "7" "4" "1" "1"
instance add75 :: Add "7" "5" "1" "2"
instance add76 :: Add "7" "6" "1" "3"
instance add77 :: Add "7" "7" "1" "4"
instance add78 :: Add "7" "8" "1" "5"
instance add79 :: Add "7" "9" "1" "6"
instance add80 :: Add "8" "0" "0" "8"
instance add81 :: Add "8" "1" "0" "9"
instance add82 :: Add "8" "2" "1" "0"
instance add83 :: Add "8" "3" "1" "1"
instance add84 :: Add "8" "4" "1" "2"
instance add85 :: Add "8" "5" "1" "3"
instance add86 :: Add "8" "6" "1" "4"
instance add87 :: Add "8" "7" "1" "5"
instance add88 :: Add "8" "8" "1" "6"
instance add89 :: Add "8" "9" "1" "7"
instance add90 :: Add "9" "0" "0" "9"
instance add91 :: Add "9" "1" "1" "0"
instance add92 :: Add "9" "2" "1" "1"
instance add93 :: Add "9" "3" "1" "2"
instance add94 :: Add "9" "4" "1" "3"
instance add95 :: Add "9" "5" "1" "4"
instance add96 :: Add "9" "6" "1" "5"
instance add97 :: Add "9" "7" "1" "6"
instance add98 :: Add "9" "8" "1" "7"
instance add99 :: Add "9" "9" "1" "8"

-- Minus
-- x, y, z \in { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"}
-- carry \in { "0", "1" }, meaning { "-0", "-1" }
class (IsDigit x, IsDigit y, IsDigit carry, IsDigit z) <=
  Minus (x :: Symbol) (y :: Symbol) (carry :: Symbol) (z :: Symbol) | x y -> carry z

instance minus00 :: Minus "0" "0" "0" "0"
instance minus01 :: Minus "0" "1" "1" "9"
instance minus02 :: Minus "0" "2" "1" "8"
instance minus03 :: Minus "0" "3" "1" "7"
instance minus04 :: Minus "0" "4" "1" "6"
instance minus05 :: Minus "0" "5" "1" "5"
instance minus06 :: Minus "0" "6" "1" "4"
instance minus07 :: Minus "0" "7" "1" "3"
instance minus08 :: Minus "0" "8" "1" "2"
instance minus09 :: Minus "0" "9" "1" "1"
instance minus10 :: Minus "1" "0" "0" "1"
instance minus11 :: Minus "1" "1" "0" "0"
instance minus12 :: Minus "1" "2" "1" "9"
instance minus13 :: Minus "1" "3" "1" "8"
instance minus14 :: Minus "1" "4" "1" "7"
instance minus15 :: Minus "1" "5" "1" "6"
instance minus16 :: Minus "1" "6" "1" "5"
instance minus17 :: Minus "1" "7" "1" "4"
instance minus18 :: Minus "1" "8" "1" "3"
instance minus19 :: Minus "1" "9" "1" "2"
instance minus20 :: Minus "2" "0" "0" "2"
instance minus21 :: Minus "2" "1" "0" "1"
instance minus22 :: Minus "2" "2" "0" "0"
instance minus23 :: Minus "2" "3" "1" "9"
instance minus24 :: Minus "2" "4" "1" "8"
instance minus25 :: Minus "2" "5" "1" "7"
instance minus26 :: Minus "2" "6" "1" "6"
instance minus27 :: Minus "2" "7" "1" "5"
instance minus28 :: Minus "2" "8" "1" "4"
instance minus29 :: Minus "2" "9" "1" "3"
instance minus30 :: Minus "3" "0" "0" "3"
instance minus31 :: Minus "3" "1" "0" "2"
instance minus32 :: Minus "3" "2" "0" "1"
instance minus33 :: Minus "3" "3" "0" "0"
instance minus34 :: Minus "3" "4" "1" "9"
instance minus35 :: Minus "3" "5" "1" "8"
instance minus36 :: Minus "3" "6" "1" "7"
instance minus37 :: Minus "3" "7" "1" "6"
instance minus38 :: Minus "3" "8" "1" "5"
instance minus39 :: Minus "3" "9" "1" "4"
instance minus40 :: Minus "4" "0" "0" "4"
instance minus41 :: Minus "4" "1" "0" "3"
instance minus42 :: Minus "4" "2" "0" "2"
instance minus43 :: Minus "4" "3" "0" "1"
instance minus44 :: Minus "4" "4" "0" "0"
instance minus45 :: Minus "4" "5" "1" "9"
instance minus46 :: Minus "4" "6" "1" "8"
instance minus47 :: Minus "4" "7" "1" "7"
instance minus48 :: Minus "4" "8" "1" "6"
instance minus49 :: Minus "4" "9" "1" "5"
instance minus50 :: Minus "5" "0" "0" "5"
instance minus51 :: Minus "5" "1" "0" "4"
instance minus52 :: Minus "5" "2" "0" "3"
instance minus53 :: Minus "5" "3" "0" "2"
instance minus54 :: Minus "5" "4" "0" "1"
instance minus55 :: Minus "5" "5" "0" "0"
instance minus56 :: Minus "5" "6" "1" "9"
instance minus57 :: Minus "5" "7" "1" "8"
instance minus58 :: Minus "5" "8" "1" "7"
instance minus59 :: Minus "5" "9" "1" "6"
instance minus60 :: Minus "6" "0" "0" "6"
instance minus61 :: Minus "6" "1" "0" "5"
instance minus62 :: Minus "6" "2" "0" "4"
instance minus63 :: Minus "6" "3" "0" "3"
instance minus64 :: Minus "6" "4" "0" "2"
instance minus65 :: Minus "6" "5" "0" "1"
instance minus66 :: Minus "6" "6" "0" "0"
instance minus67 :: Minus "6" "7" "1" "9"
instance minus68 :: Minus "6" "8" "1" "8"
instance minus69 :: Minus "6" "9" "1" "7"
instance minus70 :: Minus "7" "0" "0" "7"
instance minus71 :: Minus "7" "1" "0" "6"
instance minus72 :: Minus "7" "2" "0" "5"
instance minus73 :: Minus "7" "3" "0" "4"
instance minus74 :: Minus "7" "4" "0" "3"
instance minus75 :: Minus "7" "5" "0" "2"
instance minus76 :: Minus "7" "6" "0" "1"
instance minus77 :: Minus "7" "7" "0" "0"
instance minus78 :: Minus "7" "8" "1" "9"
instance minus79 :: Minus "7" "9" "1" "8"
instance minus80 :: Minus "8" "0" "0" "8"
instance minus81 :: Minus "8" "1" "0" "7"
instance minus82 :: Minus "8" "2" "0" "6"
instance minus83 :: Minus "8" "3" "0" "5"
instance minus84 :: Minus "8" "4" "0" "4"
instance minus85 :: Minus "8" "5" "0" "3"
instance minus86 :: Minus "8" "6" "0" "2"
instance minus87 :: Minus "8" "7" "0" "1"
instance minus88 :: Minus "8" "8" "0" "0"
instance minus89 :: Minus "8" "9" "1" "9"
instance minus90 :: Minus "9" "0" "0" "9"
instance minus91 :: Minus "9" "1" "0" "8"
instance minus92 :: Minus "9" "2" "0" "7"
instance minus93 :: Minus "9" "3" "0" "6"
instance minus94 :: Minus "9" "4" "0" "5"
instance minus95 :: Minus "9" "5" "0" "4"
instance minus96 :: Minus "9" "6" "0" "3"
instance minus97 :: Minus "9" "7" "0" "2"
instance minus98 :: Minus "9" "8" "0" "1"
instance minus99 :: Minus "9" "9" "0" "0"

-- Multiply
class (IsDigit x, IsDigit y, IsDigit carry, IsDigit z) <=
  Multiply (x :: Symbol) (y :: Symbol) (carry :: Symbol) (z :: Symbol) | x y -> carry z

instance multiply00 :: Multiply "0" "0" "0" "0"
instance multiply01 :: Multiply "0" "1" "0" "0"
instance multiply02 :: Multiply "0" "2" "0" "0"
instance multiply03 :: Multiply "0" "3" "0" "0"
instance multiply04 :: Multiply "0" "4" "0" "0"
instance multiply05 :: Multiply "0" "5" "0" "0"
instance multiply06 :: Multiply "0" "6" "0" "0"
instance multiply07 :: Multiply "0" "7" "0" "0"
instance multiply08 :: Multiply "0" "8" "0" "0"
instance multiply09 :: Multiply "0" "9" "0" "0"
instance multiply10 :: Multiply "1" "0" "0" "0"
instance multiply11 :: Multiply "1" "1" "0" "1"
instance multiply12 :: Multiply "1" "2" "0" "2"
instance multiply13 :: Multiply "1" "3" "0" "3"
instance multiply14 :: Multiply "1" "4" "0" "4"
instance multiply15 :: Multiply "1" "5" "0" "5"
instance multiply16 :: Multiply "1" "6" "0" "6"
instance multiply17 :: Multiply "1" "7" "0" "7"
instance multiply18 :: Multiply "1" "8" "0" "8"
instance multiply19 :: Multiply "1" "9" "0" "9"
instance multiply20 :: Multiply "2" "0" "0" "0"
instance multiply21 :: Multiply "2" "1" "0" "2"
instance multiply22 :: Multiply "2" "2" "0" "4"
instance multiply23 :: Multiply "2" "3" "0" "6"
instance multiply24 :: Multiply "2" "4" "0" "8"
instance multiply25 :: Multiply "2" "5" "1" "0"
instance multiply26 :: Multiply "2" "6" "1" "2"
instance multiply27 :: Multiply "2" "7" "1" "4"
instance multiply28 :: Multiply "2" "8" "1" "6"
instance multiply29 :: Multiply "2" "9" "1" "8"
instance multiply30 :: Multiply "3" "0" "0" "0"
instance multiply31 :: Multiply "3" "1" "0" "3"
instance multiply32 :: Multiply "3" "2" "0" "6"
instance multiply33 :: Multiply "3" "3" "0" "9"
instance multiply34 :: Multiply "3" "4" "1" "2"
instance multiply35 :: Multiply "3" "5" "1" "5"
instance multiply36 :: Multiply "3" "6" "1" "8"
instance multiply37 :: Multiply "3" "7" "2" "1"
instance multiply38 :: Multiply "3" "8" "2" "4"
instance multiply39 :: Multiply "3" "9" "2" "7"
instance multiply40 :: Multiply "4" "0" "0" "0"
instance multiply41 :: Multiply "4" "1" "0" "4"
instance multiply42 :: Multiply "4" "2" "0" "8"
instance multiply43 :: Multiply "4" "3" "1" "2"
instance multiply44 :: Multiply "4" "4" "1" "6"
instance multiply45 :: Multiply "4" "5" "2" "0"
instance multiply46 :: Multiply "4" "6" "2" "4"
instance multiply47 :: Multiply "4" "7" "2" "8"
instance multiply48 :: Multiply "4" "8" "3" "2"
instance multiply49 :: Multiply "4" "9" "3" "6"
instance multiply50 :: Multiply "5" "0" "0" "0"
instance multiply51 :: Multiply "5" "1" "0" "5"
instance multiply52 :: Multiply "5" "2" "1" "0"
instance multiply53 :: Multiply "5" "3" "1" "5"
instance multiply54 :: Multiply "5" "4" "2" "0"
instance multiply55 :: Multiply "5" "5" "2" "5"
instance multiply56 :: Multiply "5" "6" "3" "0"
instance multiply57 :: Multiply "5" "7" "3" "5"
instance multiply58 :: Multiply "5" "8" "4" "0"
instance multiply59 :: Multiply "5" "9" "4" "5"
instance multiply60 :: Multiply "6" "0" "0" "0"
instance multiply61 :: Multiply "6" "1" "0" "6"
instance multiply62 :: Multiply "6" "2" "1" "2"
instance multiply63 :: Multiply "6" "3" "1" "8"
instance multiply64 :: Multiply "6" "4" "2" "4"
instance multiply65 :: Multiply "6" "5" "3" "0"
instance multiply66 :: Multiply "6" "6" "3" "6"
instance multiply67 :: Multiply "6" "7" "4" "2"
instance multiply68 :: Multiply "6" "8" "4" "8"
instance multiply69 :: Multiply "6" "9" "5" "4"
instance multiply70 :: Multiply "7" "0" "0" "0"
instance multiply71 :: Multiply "7" "1" "0" "7"
instance multiply72 :: Multiply "7" "2" "1" "4"
instance multiply73 :: Multiply "7" "3" "2" "1"
instance multiply74 :: Multiply "7" "4" "2" "8"
instance multiply75 :: Multiply "7" "5" "3" "5"
instance multiply76 :: Multiply "7" "6" "4" "2"
instance multiply77 :: Multiply "7" "7" "4" "9"
instance multiply78 :: Multiply "7" "8" "5" "6"
instance multiply79 :: Multiply "7" "9" "6" "3"
instance multiply80 :: Multiply "8" "0" "0" "0"
instance multiply81 :: Multiply "8" "1" "0" "8"
instance multiply82 :: Multiply "8" "2" "1" "6"
instance multiply83 :: Multiply "8" "3" "2" "4"
instance multiply84 :: Multiply "8" "4" "3" "2"
instance multiply85 :: Multiply "8" "5" "4" "0"
instance multiply86 :: Multiply "8" "6" "4" "8"
instance multiply87 :: Multiply "8" "7" "5" "6"
instance multiply88 :: Multiply "8" "8" "6" "4"
instance multiply89 :: Multiply "8" "9" "7" "2"
instance multiply90 :: Multiply "9" "0" "0" "0"
instance multiply91 :: Multiply "9" "1" "0" "9"
instance multiply92 :: Multiply "9" "2" "1" "8"
instance multiply93 :: Multiply "9" "3" "2" "7"
instance multiply94 :: Multiply "9" "4" "3" "6"
instance multiply95 :: Multiply "9" "5" "4" "5"
instance multiply96 :: Multiply "9" "6" "5" "4"
instance multiply97 :: Multiply "9" "7" "6" "3"
instance multiply98 :: Multiply "9" "8" "7" "2"
instance multiply99 :: Multiply "9" "9" "8" "1"

-- | Divide

class (IsDigit x0, IsDigit x1, IsDigit y, IsDigit z, IsDigit remainder) <=
  Divide (x0 :: Symbol) (x1 :: Symbol) (y :: Symbol) (z :: Symbol) (remainder :: Symbol) | x0 x1 y -> z remainder

instance dividex0 ::
  ( Fail (Text "divide by zero is not defined")
  , IsDigit x0, IsDigit x1, IsDigit z, IsDigit remainder
  )
  => Divide x0 x1 "0" z remainder
instance divide00_1 :: Divide "0""0" "1" "0" "0"
instance divide01_1 :: Divide "0""1" "1" "1" "0"
instance divide02_1 :: Divide "0""2" "1" "2" "0"
instance divide03_1 :: Divide "0""3" "1" "3" "0"
instance divide04_1 :: Divide "0""4" "1" "4" "0"
instance divide05_1 :: Divide "0""5" "1" "5" "0"
instance divide06_1 :: Divide "0""6" "1" "6" "0"
instance divide07_1 :: Divide "0""7" "1" "7" "0"
instance divide08_1 :: Divide "0""8" "1" "8" "0"
instance divide09_1 :: Divide "0""9" "1" "9" "0"
instance divide00_2 :: Divide "0""0" "2" "0" "0"
instance divide01_2 :: Divide "0""1" "2" "0" "1"
instance divide02_2 :: Divide "0""2" "2" "1" "0"
instance divide03_2 :: Divide "0""3" "2" "1" "1"
instance divide04_2 :: Divide "0""4" "2" "2" "0"
instance divide05_2 :: Divide "0""5" "2" "2" "1"
instance divide06_2 :: Divide "0""6" "2" "3" "0"
instance divide07_2 :: Divide "0""7" "2" "3" "1"
instance divide08_2 :: Divide "0""8" "2" "4" "0"
instance divide09_2 :: Divide "0""9" "2" "4" "1"
instance divide10_2 :: Divide "1""0" "2" "5" "0"
instance divide00_3 :: Divide "0""0" "3" "0" "0"
instance divide01_3 :: Divide "0""1" "3" "0" "1"
instance divide02_3 :: Divide "0""2" "3" "0" "2"
instance divide03_3 :: Divide "0""3" "3" "1" "0"
instance divide04_3 :: Divide "0""4" "3" "1" "1"
instance divide05_3 :: Divide "0""5" "3" "1" "2"
instance divide06_3 :: Divide "0""6" "3" "2" "0"
instance divide07_3 :: Divide "0""7" "3" "2" "1"
instance divide08_3 :: Divide "0""8" "3" "2" "2"
instance divide09_3 :: Divide "0""9" "3" "3" "0"
instance divide10_3 :: Divide "1""0" "3" "3" "1"
instance divide20_3 :: Divide "2""0" "3" "6" "2"
instance divide00_4 :: Divide "0""0" "4" "0" "0"
instance divide01_4 :: Divide "0""1" "4" "0" "1"
instance divide02_4 :: Divide "0""2" "4" "0" "2"
instance divide03_4 :: Divide "0""3" "4" "0" "3"
instance divide04_4 :: Divide "0""4" "4" "1" "0"
instance divide05_4 :: Divide "0""5" "4" "1" "1"
instance divide06_4 :: Divide "0""6" "4" "1" "2"
instance divide07_4 :: Divide "0""7" "4" "1" "3"
instance divide08_4 :: Divide "0""8" "4" "2" "0"
instance divide09_4 :: Divide "0""9" "4" "2" "1"
instance divide10_4 :: Divide "1""0" "4" "2" "2"
instance divide20_4 :: Divide "2""0" "4" "5" "0"
instance divide30_4 :: Divide "3""0" "4" "7" "2"
instance divide00_5 :: Divide "0""0" "5" "0" "0"
instance divide01_5 :: Divide "0""1" "5" "0" "1"
instance divide02_5 :: Divide "0""2" "5" "0" "2"
instance divide03_5 :: Divide "0""3" "5" "0" "3"
instance divide04_5 :: Divide "0""4" "5" "0" "4"
instance divide05_5 :: Divide "0""5" "5" "1" "0"
instance divide06_5 :: Divide "0""6" "5" "1" "1"
instance divide07_5 :: Divide "0""7" "5" "1" "2"
instance divide08_5 :: Divide "0""8" "5" "1" "3"
instance divide09_5 :: Divide "0""9" "5" "1" "4"
instance divide10_5 :: Divide "1""0" "5" "2" "0"
instance divide20_5 :: Divide "2""0" "5" "4" "0"
instance divide40_5 :: Divide "4""0" "5" "7" "5"
instance divide30_5 :: Divide "3""0" "5" "6" "0"
instance divide00_6 :: Divide "0""0" "6" "0" "0"
instance divide01_6 :: Divide "0""1" "6" "0" "1"
instance divide02_6 :: Divide "0""2" "6" "0" "2"
instance divide03_6 :: Divide "0""3" "6" "0" "3"
instance divide04_6 :: Divide "0""4" "6" "0" "4"
instance divide05_6 :: Divide "0""5" "6" "0" "5"
instance divide06_6 :: Divide "0""6" "6" "1" "0"
instance divide07_6 :: Divide "0""7" "6" "1" "1"
instance divide08_6 :: Divide "0""8" "6" "1" "2"
instance divide09_6 :: Divide "0""9" "6" "1" "3"
instance divide10_6 :: Divide "1""0" "6" "1" "4"
instance divide20_6 :: Divide "2""0" "6" "3" "2"
instance divide30_6 :: Divide "3""0" "6" "5" "0"
instance divide40_6 :: Divide "4""0" "6" "6" "4"
instance divide50_6 :: Divide "5""0" "6" "8" "2"
instance divide00_7 :: Divide "0""0" "7" "0" "0"
instance divide01_7 :: Divide "0""1" "7" "0" "1"
instance divide02_7 :: Divide "0""2" "7" "0" "2"
instance divide03_7 :: Divide "0""3" "7" "0" "3"
instance divide04_7 :: Divide "0""4" "7" "0" "4"
instance divide05_7 :: Divide "0""5" "7" "0" "5"
instance divide06_7 :: Divide "0""6" "7" "0" "6"
instance divide07_7 :: Divide "0""7" "7" "1" "0"
instance divide08_7 :: Divide "0""8" "7" "1" "1"
instance divide09_7 :: Divide "0""9" "7" "1" "2"
instance divide10_7 :: Divide "1""0" "7" "1" "3"
instance divide20_7 :: Divide "2""0" "7" "2" "6"
instance divide30_7 :: Divide "3""0" "7" "4" "2"
instance divide40_7 :: Divide "4""0" "7" "5" "5"
instance divide50_7 :: Divide "5""0" "7" "7" "1"
instance divide60_7 :: Divide "6""0" "7" "8" "4"
instance divide00_8 :: Divide "0""0" "8" "0" "0"
instance divide01_8 :: Divide "0""1" "8" "0" "1"
instance divide02_8 :: Divide "0""2" "8" "0" "2"
instance divide03_8 :: Divide "0""3" "8" "0" "3"
instance divide04_8 :: Divide "0""4" "8" "0" "4"
instance divide05_8 :: Divide "0""5" "8" "0" "5"
instance divide06_8 :: Divide "0""6" "8" "0" "6"
instance divide07_8 :: Divide "0""7" "8" "0" "7"
instance divide08_8 :: Divide "0""8" "8" "1" "0"
instance divide09_8 :: Divide "0""9" "8" "1" "1"
instance divide10_8 :: Divide "1""0" "8" "1" "2"
instance divide20_8 :: Divide "2""0" "8" "2" "4"
instance divide30_8 :: Divide "3""0" "8" "3" "6"
instance divide40_8 :: Divide "4""0" "8" "5" "0"
instance divide50_8 :: Divide "5""0" "8" "6" "2"
instance divide60_8 :: Divide "6""0" "8" "7" "4"
instance divide70_8 :: Divide "7""0" "8" "8" "6"
instance divide00_9 :: Divide "0""0" "9" "0" "0"
instance divide01_9 :: Divide "0""1" "9" "0" "1"
instance divide02_9 :: Divide "0""2" "9" "0" "2"
instance divide03_9 :: Divide "0""3" "9" "0" "3"
instance divide04_9 :: Divide "0""4" "9" "0" "4"
instance divide05_9 :: Divide "0""5" "9" "0" "5"
instance divide06_9 :: Divide "0""6" "9" "0" "6"
instance divide07_9 :: Divide "0""7" "9" "0" "7"
instance divide08_9 :: Divide "0""8" "9" "0" "8"
instance divide09_9 :: Divide "0""9" "9" "1" "0"
instance divide10_9 :: Divide "1""0" "9" "1" "1"
instance divide20_9 :: Divide "2""0" "9" "2" "2"
instance divide30_9 :: Divide "3""0" "9" "3" "3"
instance divide40_9 :: Divide "4""0" "9" "4" "4"
instance divide50_9 :: Divide "5""0" "9" "5" "5"
instance divide60_9 :: Divide "6""0" "9" "6" "6"
instance divide70_9 :: Divide "7""0" "9" "7" "7"
instance divide80_9 :: Divide "8""0" "9" "8" "8"
