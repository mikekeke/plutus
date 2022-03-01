module Size.Eq (test) where

import PlutusTx.Code (CompiledCode)
import PlutusTx.Eq qualified as Eq
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import PlutusTx.Test (fitsInto)
import Test.Tasty (TestTree, testGroup)


test ::TestTree
test =
  testGroup "Eq" [
    fitsInto "Integer" cEqInteger 20,
    fitsInto "BuiltinByteString" cEqInteger 20,
    fitsInto "BuiltinData" cEqInteger 20,
    fitsInto "BuiltinString" cEqBuiltinString 20,
    fitsInto "[]" cEqList 111,
    fitsInto "Bool" cEqBool 25,
    fitsInto "Maybe" cEqMaybe 53,
    fitsInto "Either" cEqEither 69,
    fitsInto "()" cEqUnit 25,
    fitsInto "(,)" cEqTuple 66

  ]

cEqInteger :: CompiledCode (Integer -> Integer -> Bool)
cEqInteger = $$(compile [|| (Eq.==) ||])

cEqBuiltinByteString :: CompiledCode (BuiltinByteString -> BuiltinByteString -> Bool)
cEqBuiltinByteString = $$(compile [|| (Eq.==) ||])

cEqBuiltinData :: CompiledCode (BuiltinData -> BuiltinData -> Bool)
cEqBuiltinData = $$(compile [|| (Eq.==) ||])

cEqBuiltinString :: CompiledCode (BuiltinString -> BuiltinString -> Bool)
cEqBuiltinString = $$(compile [|| (Eq.==) ||])

cEqList :: CompiledCode ([Integer] -> [Integer] -> Bool)
cEqList = $$(compile [|| (Eq.==) ||])

cEqBool :: CompiledCode (Bool -> Bool -> Bool)
cEqBool = $$(compile [|| (Eq.==) ||])

cEqMaybe :: CompiledCode (Maybe Integer -> Maybe Integer -> Bool)
cEqMaybe = $$(compile [|| (Eq.==) ||])

type EitherInt = Either Integer Integer
cEqEither :: CompiledCode (EitherInt -> EitherInt -> Bool)
cEqEither = $$(compile [|| (Eq.==) ||])

cEqUnit :: CompiledCode (Bool -> Bool -> Bool)
cEqUnit = $$(compile [|| (Eq.==) ||])

type TupleInt = (Integer, Integer)
cEqTuple :: CompiledCode (TupleInt -> TupleInt -> Bool)
cEqTuple = $$(compile [|| (Eq.==) ||])
