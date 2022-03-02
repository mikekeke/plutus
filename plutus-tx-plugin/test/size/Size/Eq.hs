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
    testGroup "Integer" [
      fitsInto "==" cEqInteger 20,
      fitsInto "/=" cNeqInteger 34
      ],
    testGroup "BuiltinByteString" [
      fitsInto "==" cEqBuiltinByteString 20,
      fitsInto "/=" cNeqBuiltinByteString 34
      ],
    testGroup "BuiltinData" [
      fitsInto "==" cEqBuiltinData 20,
      fitsInto "/=" cNeqBuiltinData 34
      ],
    testGroup "BuiltinString" [
      fitsInto "==" cEqBuiltinString 20,
      fitsInto "/=" cNeqBuiltinString 34
      ],
    testGroup "List" [
      fitsInto "==" cEqList 111,
      fitsInto "/=" cNeqList 125
      ],
    testGroup "Bool" [
      fitsInto "==" cEqBool 25,
      fitsInto "/=" cNeqBool 39
      ],
    testGroup "Maybe" [
      fitsInto "==" cEqMaybe 53,
      fitsInto "/=" cNeqMaybe 61
      ],
    testGroup "Either" [
      fitsInto "==" cEqEither 69,
      fitsInto "/=" cNeqEither 86
      ],
    testGroup "()" [
      fitsInto "==" cEqUnit 6,
      fitsInto "/=" cNeqUnit 20
      ],
    testGroup "(,)" [
      fitsInto "==" cEqTuple 66,
      fitsInto "/=" cNeqTuple 83
      ]
  ]

-- Integer

cEqInteger :: CompiledCode (Integer -> Integer -> Bool)
cEqInteger = $$(compile [|| (Eq.==) ||])

cNeqInteger :: CompiledCode (Integer -> Integer -> Bool)
cNeqInteger = $$(compile [|| (Eq./=) ||])

-- BuiltinByteString

cEqBuiltinByteString :: CompiledCode (BuiltinByteString -> BuiltinByteString -> Bool)
cEqBuiltinByteString = $$(compile [|| (Eq.==) ||])

cNeqBuiltinByteString :: CompiledCode (BuiltinByteString -> BuiltinByteString -> Bool)
cNeqBuiltinByteString = $$(compile [|| (Eq./=) ||])

-- BuiltinData

cEqBuiltinData :: CompiledCode (BuiltinData -> BuiltinData -> Bool)
cEqBuiltinData = $$(compile [|| (Eq.==) ||])

cNeqBuiltinData :: CompiledCode (BuiltinData -> BuiltinData -> Bool)
cNeqBuiltinData = $$(compile [|| (Eq./=) ||])

-- BuiltinString

cEqBuiltinString :: CompiledCode (BuiltinString -> BuiltinString -> Bool)
cEqBuiltinString = $$(compile [|| (Eq.==) ||])

cNeqBuiltinString :: CompiledCode (BuiltinString -> BuiltinString -> Bool)
cNeqBuiltinString = $$(compile [|| (Eq./=) ||])

-- List

cEqList :: CompiledCode ([Integer] -> [Integer] -> Bool)
cEqList = $$(compile [|| (Eq.==) ||])

cNeqList :: CompiledCode ([Integer] -> [Integer] -> Bool)
cNeqList = $$(compile [|| (Eq./=) ||])

-- Bool

cEqBool :: CompiledCode (Bool -> Bool -> Bool)
cEqBool = $$(compile [|| (Eq.==) ||])

cNeqBool :: CompiledCode (Bool -> Bool -> Bool)
cNeqBool = $$(compile [|| (Eq./=) ||])

-- Maybe

cEqMaybe :: CompiledCode (Maybe Integer -> Maybe Integer -> Bool)
cEqMaybe = $$(compile [|| (Eq.==) ||])

cNeqMaybe :: CompiledCode (Maybe Integer -> Maybe Integer -> Bool)
cNeqMaybe = $$(compile [|| (Eq./=) ||])

-- Either

type EitherInt = Either Integer Integer
cEqEither :: CompiledCode (EitherInt -> EitherInt -> Bool)
cEqEither = $$(compile [|| (Eq.==) ||])

cNeqEither :: CompiledCode (EitherInt -> EitherInt -> Bool)
cNeqEither = $$(compile [|| (Eq./=) ||])

-- Unit

cEqUnit :: CompiledCode (() -> () -> Bool)
cEqUnit = $$(compile [|| (Eq.==) ||])

cNeqUnit :: CompiledCode (() -> () -> Bool)
cNeqUnit = $$(compile [|| (Eq./=) ||])

-- Tuple

type TupleInt = (Integer, Integer)
cEqTuple :: CompiledCode (TupleInt -> TupleInt -> Bool)
cEqTuple = $$(compile [|| (Eq.==) ||])

cNeqTuple :: CompiledCode (TupleInt -> TupleInt -> Bool)
cNeqTuple = $$(compile [|| (Eq./=) ||])
