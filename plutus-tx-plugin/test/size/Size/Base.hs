module Size.Base (test) where

import PlutusTx.Base qualified as Base
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import PlutusTx.Test (fitsInto)
import Test.Tasty (TestTree, testGroup)

test :: TestTree
test =
  testGroup "Base" [
    fitsInto "fst" cFst 7,
    fitsInto "snd" cSnd 7,
    fitsInto "curry" cCurry 12,
    fitsInto "uncurry" cUncurry 12,
    fitsInto "$" cApply 5,
    fitsInto "flip" cFlip 8,
    fitsInto "until" cUntil 35,
    fitsInto "." cCompose 8,
    fitsInto "const" cConst 3,
    fitsInto "id" cId 2
  ]

cFst :: CompiledCode ((Integer, Integer) -> Integer)
cFst = $$(compile [|| Base.fst ||])

cSnd :: CompiledCode ((Integer, Integer) -> Integer)
cSnd = $$(compile [|| Base.snd ||])

cCurry :: CompiledCode (((Integer, Integer) -> Integer) -> Integer -> Integer -> Integer)
cCurry = $$(compile [|| Base.curry ||])

cUncurry :: CompiledCode ((Integer -> Integer -> Integer) -> (Integer, Integer) -> Integer)
cUncurry = $$(compile [|| Base.uncurry ||])

cApply :: CompiledCode ((Integer -> Integer) -> Integer -> Integer)
cApply = $$(compile [|| (Base.$) ||])

cFlip :: CompiledCode ((Integer -> Integer -> Integer) -> Integer -> Integer -> Integer)
cFlip = $$(compile [|| Base.flip ||])

cUntil :: CompiledCode ((Integer -> Bool) -> (Integer -> Integer) -> Integer -> Integer)
cUntil = $$(compile [|| Base.until ||])

cCompose :: CompiledCode ((Integer -> Integer) -> (Integer -> Integer) -> Integer -> Integer)
cCompose = $$(compile [|| (Base..) ||])

cConst :: CompiledCode (Integer -> Integer -> Integer)
cConst = $$(compile [|| Base.const ||])

cId :: CompiledCode (Integer -> Integer)
cId = $$(compile [|| Base.id ||])
