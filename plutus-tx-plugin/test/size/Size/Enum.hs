module Size.Enum (test) where

import PlutusTx.Code (CompiledCode)
import PlutusTx.Enum qualified as Enum
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import PlutusTx.Test (fitsInto)
import Test.Tasty (TestTree, testGroup)

test ::TestTree
test =
  testGroup "Enum" [
    testGroup "Integer" [
      fitsInto "succ" cSuccInt 6,
      fitsInto "pred" cPredInt 6,
      fitsInto "toEnum" cToEnumInt 6,
      fitsInto "fromEnum" cFromEnumInt 6
    ],
    testGroup "Unit" [
      fitsInto "succ" cSuccUnit 15,
      fitsInto "pred" cPredUnit 15,
      fitsInto "toEnum" cToEnumUnit 15,
      fitsInto "fromEnum" cFromEnumUnit 15
    ],
    testGroup "Bool" [
      fitsInto "succ" cSuccBool 26,
      fitsInto "pred" cPredBool 26,
      fitsInto "toEnum" cToEnumBool 26,
      fitsInto "fromEnum" cFromEnumBool 26
    ],
    testGroup "Ordering" [
      fitsInto "succ" cSuccBool 26,
      fitsInto "pred" cPredBool 26,
      fitsInto "toEnum" cToEnumBool 26,
      fitsInto "fromEnum" cFromEnumBool 26
    ]
  ]
-- Integer
cSuccInt :: CompiledCode (Integer -> Integer)
cSuccInt = $$(compile [|| Enum.succ ||])

cPredInt :: CompiledCode (Integer -> Integer)
cPredInt = $$(compile [|| Enum.pred ||])

cToEnumInt :: CompiledCode (Integer -> Integer)
cToEnumInt = $$(compile [|| Enum.pred ||])

cFromEnumInt :: CompiledCode (Integer -> Integer)
cFromEnumInt = $$(compile [|| Enum.pred ||])

-- Unit
cSuccUnit :: CompiledCode (() -> ())
cSuccUnit = $$(compile [|| Enum.succ ||])

cPredUnit :: CompiledCode (() -> ())
cPredUnit = $$(compile [|| Enum.pred ||])

cToEnumUnit :: CompiledCode (() -> ())
cToEnumUnit = $$(compile [|| Enum.pred ||])

cFromEnumUnit :: CompiledCode (() -> ())
cFromEnumUnit = $$(compile [|| Enum.pred ||])

-- Bool
cSuccBool :: CompiledCode (Bool -> Bool)
cSuccBool = $$(compile [|| Enum.succ ||])

cPredBool :: CompiledCode (Bool -> Bool)
cPredBool = $$(compile [|| Enum.pred ||])

cToEnumBool :: CompiledCode (Bool -> Bool)
cToEnumBool = $$(compile [|| Enum.pred ||])

cFromEnumBool :: CompiledCode (Bool -> Bool)
cFromEnumBool = $$(compile [|| Enum.pred ||])

-- Ordering
cSuccOrdering :: CompiledCode (Ordering -> Ordering)
cSuccOrdering = $$(compile [|| Enum.succ ||])

cPredOrdering :: CompiledCode (Ordering -> Ordering)
cPredOrdering = $$(compile [|| Enum.pred ||])

cToEnumOrdering :: CompiledCode (Ordering -> Ordering)
cToEnumOrdering = $$(compile [|| Enum.pred ||])

cFromEnumOrdering :: CompiledCode (Ordering -> Ordering)
cFromEnumOrdering = $$(compile [|| Enum.pred ||])
