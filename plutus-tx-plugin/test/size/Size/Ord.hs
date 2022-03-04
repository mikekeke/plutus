module Size.Ord (test) where

import PlutusTx.Code (CompiledCode)
import PlutusTx.Ord qualified as Ord
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import PlutusTx.Test (fitsInto)
import Test.Tasty (TestTree, testGroup)

test ::TestTree
test =
  testGroup "Ord" [
    testGroup "Integer" [
      fitsInto "compare" cIntCompare 65,
      fitsInto "<" cIntLt 20,
      fitsInto "<=" cIntLte 20,
      fitsInto ">" cIntGt 20,
      fitsInto ">=" cIntGte 20,
      fitsInto "min" cIntMax 28,
      fitsInto "max" cIntMin 28
    ],
    testGroup "BuiltinByteString" [
      fitsInto "compare" cBuiltinByteStringCompare 65,
      fitsInto "<" cBuiltinByteStringLt 20,
      fitsInto "<=" cBuiltinByteStringLte 20,
      fitsInto ">" cBuiltinByteStringGt 20,
      fitsInto ">=" cBuiltinByteStringGte 20,
      fitsInto "min" cBuiltinByteStringMax 28,
      fitsInto "max" cBuiltinByteStringMin 28
    ],
    testGroup "List" [
      fitsInto "compare" cListCompare 302,
      fitsInto "<" cListLt 319,
      fitsInto "<=" cListLte 319,
      fitsInto ">" cListGt 319,
      fitsInto ">=" cListGte 319,
      fitsInto "min" cListMax 319,
      fitsInto "max" cListMin 319
    ],
    testGroup "Bool" [
      fitsInto "compare" cBoolCompare 42,
      fitsInto "<" cBoolLt 14,
      fitsInto "<=" cBoolLte 14,
      fitsInto ">" cBoolGt 28,
      fitsInto ">=" cBoolGte 28,
      fitsInto "min" cBoolMax 14,
      fitsInto "max" cBoolMin 14
    ],
    testGroup "Maybe" [
      fitsInto "compare" cMaybeCompare 101,
      fitsInto "<" cMaybeLt 103,
      fitsInto "<=" cMaybeLte 94,
      fitsInto ">" cMaybeGt 94,
      fitsInto ">=" cMaybeGte 103,
      fitsInto "min" cMaybeMax 94,
      fitsInto "max" cMaybeMin 103
    ],
    testGroup "Either" [
      fitsInto "compare" cEitherCompare 261,
      fitsInto "<" cEitherLt 277,
      fitsInto "<=" cEitherLte 277,
      fitsInto ">" cEitherGt 277,
      fitsInto ">=" cEitherGte 277,
      fitsInto "min" cEitherMax 277,
      fitsInto "max" cEitherMin 277
    ],
    testGroup "()" [
      fitsInto "compare" cUnitCompare 7,
      fitsInto "<" cUnitLt 6,
      fitsInto "<=" cUnitLte 6,
      fitsInto ">" cUnitGt 6,
      fitsInto ">=" cUnitGte 6,
      fitsInto "min" cUnitMax 3,
      fitsInto "max" cUnitMin 3
    ],
    testGroup "Tuple" [
      fitsInto "compare" cTupleCompare 268,
      fitsInto "<" cTupleLt 279,
      fitsInto "<=" cTupleLte 279,
      fitsInto ">" cTupleGt 279,
      fitsInto ">=" cTupleGte 279,
      fitsInto "min" cTupleMax 279,
      fitsInto "max" cTupleMin 279
    ]
  ]

-- Integer
cIntCompare :: CompiledCode (Integer -> Integer -> Ordering)
cIntCompare = $$(compile [|| Ord.compare ||])

cIntLt :: CompiledCode (Integer -> Integer -> Bool)
cIntLt = $$(compile [|| (Ord.<) ||])

cIntLte :: CompiledCode (Integer -> Integer -> Bool)
cIntLte = $$(compile [|| (Ord.<=) ||])

cIntGt :: CompiledCode (Integer -> Integer -> Bool)
cIntGt = $$(compile [|| (Ord.>) ||])

cIntGte :: CompiledCode (Integer -> Integer -> Bool)
cIntGte = $$(compile [|| (Ord.>=) ||])

cIntMax :: CompiledCode (Integer -> Integer -> Integer)
cIntMax = $$(compile [|| Ord.max ||])

cIntMin :: CompiledCode (Integer -> Integer -> Integer)
cIntMin = $$(compile [|| Ord.min ||])

-- BuiltinByteString
cBuiltinByteStringCompare :: CompiledCode (BuiltinByteString -> BuiltinByteString -> Ordering)
cBuiltinByteStringCompare = $$(compile [|| Ord.compare ||])

cBuiltinByteStringLt :: CompiledCode (BuiltinByteString -> BuiltinByteString -> Bool)
cBuiltinByteStringLt = $$(compile [|| (Ord.<) ||])

cBuiltinByteStringLte :: CompiledCode (BuiltinByteString -> BuiltinByteString -> Bool)
cBuiltinByteStringLte = $$(compile [|| (Ord.<=) ||])

cBuiltinByteStringGt :: CompiledCode (BuiltinByteString -> BuiltinByteString -> Bool)
cBuiltinByteStringGt = $$(compile [|| (Ord.>) ||])

cBuiltinByteStringGte :: CompiledCode (BuiltinByteString -> BuiltinByteString -> Bool)
cBuiltinByteStringGte = $$(compile [|| (Ord.>=) ||])

cBuiltinByteStringMax :: CompiledCode (BuiltinByteString -> BuiltinByteString -> BuiltinByteString)
cBuiltinByteStringMax = $$(compile [|| Ord.max ||])

cBuiltinByteStringMin :: CompiledCode (BuiltinByteString -> BuiltinByteString -> BuiltinByteString)
cBuiltinByteStringMin = $$(compile [|| Ord.min ||])

-- List
cListCompare :: CompiledCode ([Integer] -> [Integer] -> Ordering)
cListCompare = $$(compile [|| Ord.compare ||])

cListLt :: CompiledCode ([Integer] -> [Integer] -> Bool)
cListLt = $$(compile [|| (Ord.<) ||])

cListLte :: CompiledCode ([Integer] -> [Integer] -> Bool)
cListLte = $$(compile [|| (Ord.<=) ||])

cListGt :: CompiledCode ([Integer] -> [Integer] -> Bool)
cListGt = $$(compile [|| (Ord.>) ||])

cListGte :: CompiledCode ([Integer] -> [Integer] -> Bool)
cListGte = $$(compile [|| (Ord.>=) ||])

cListMax :: CompiledCode ([Integer] -> [Integer] -> [Integer])
cListMax = $$(compile [|| Ord.max ||])

cListMin :: CompiledCode ([Integer] -> [Integer] -> [Integer])
cListMin = $$(compile [|| Ord.min ||])

-- Bool
cBoolCompare :: CompiledCode (Bool -> Bool -> Ordering)
cBoolCompare = $$(compile [|| Ord.compare ||])

cBoolLt :: CompiledCode (Bool -> Bool -> Bool)
cBoolLt = $$(compile [|| (Ord.<) ||])

cBoolLte :: CompiledCode (Bool -> Bool -> Bool)
cBoolLte = $$(compile [|| (Ord.<=) ||])

cBoolGt :: CompiledCode (Bool -> Bool -> Bool)
cBoolGt = $$(compile [|| (Ord.>) ||])

cBoolGte :: CompiledCode (Bool -> Bool -> Bool)
cBoolGte = $$(compile [|| (Ord.>=) ||])

cBoolMax :: CompiledCode (Bool -> Bool -> Bool)
cBoolMax = $$(compile [|| Ord.max ||])

cBoolMin :: CompiledCode (Bool -> Bool -> Bool)
cBoolMin = $$(compile [|| Ord.min ||])

-- Maybe
cMaybeCompare :: CompiledCode (Maybe Integer -> Maybe Integer -> Ordering)
cMaybeCompare = $$(compile [|| Ord.compare ||])

cMaybeLt :: CompiledCode (Maybe Integer -> Maybe Integer -> Bool)
cMaybeLt = $$(compile [|| (Ord.<) ||])

cMaybeLte :: CompiledCode (Maybe Integer -> Maybe Integer -> Bool)
cMaybeLte = $$(compile [|| (Ord.<=) ||])

cMaybeGt :: CompiledCode (Maybe Integer -> Maybe Integer -> Bool)
cMaybeGt = $$(compile [|| (Ord.>) ||])

cMaybeGte :: CompiledCode (Maybe Integer -> Maybe Integer -> Bool)
cMaybeGte = $$(compile [|| (Ord.>=) ||])

cMaybeMax :: CompiledCode (Maybe Integer -> Maybe Integer -> Maybe Integer)
cMaybeMax = $$(compile [|| Ord.max ||])

cMaybeMin :: CompiledCode (Maybe Integer -> Maybe Integer -> Maybe Integer)
cMaybeMin = $$(compile [|| Ord.min ||])

-- Either
type EitherInt = Either Integer Integer -- to keep signatures more readable
cEitherCompare :: CompiledCode (EitherInt -> EitherInt -> Ordering)
cEitherCompare = $$(compile [|| Ord.compare ||])

cEitherLt :: CompiledCode (EitherInt -> EitherInt -> Bool)
cEitherLt = $$(compile [|| (Ord.<) ||])

cEitherLte :: CompiledCode (EitherInt -> EitherInt -> Bool)
cEitherLte = $$(compile [|| (Ord.<=) ||])

cEitherGt :: CompiledCode (EitherInt -> EitherInt -> Bool)
cEitherGt = $$(compile [|| (Ord.>) ||])

cEitherGte :: CompiledCode (EitherInt -> EitherInt -> Bool)
cEitherGte = $$(compile [|| (Ord.>=) ||])

cEitherMax :: CompiledCode (EitherInt -> EitherInt -> EitherInt)
cEitherMax = $$(compile [|| Ord.max ||])

cEitherMin :: CompiledCode (EitherInt -> EitherInt -> EitherInt)
cEitherMin = $$(compile [|| Ord.min ||])

-- Unit
cUnitCompare :: CompiledCode (() -> () -> Ordering)
cUnitCompare = $$(compile [|| Ord.compare ||])

cUnitLt :: CompiledCode (() -> () -> Bool)
cUnitLt = $$(compile [|| (Ord.<) ||])

cUnitLte :: CompiledCode (() -> () -> Bool)
cUnitLte = $$(compile [|| (Ord.<=) ||])

cUnitGt :: CompiledCode (() -> () -> Bool)
cUnitGt = $$(compile [|| (Ord.>) ||])

cUnitGte :: CompiledCode (() -> () -> Bool)
cUnitGte = $$(compile [|| (Ord.>=) ||])

cUnitMax :: CompiledCode (() -> () -> ())
cUnitMax = $$(compile [|| Ord.max ||])

cUnitMin :: CompiledCode (() -> () -> ())
cUnitMin = $$(compile [|| Ord.min ||])

-- Tuple
type TupleInt = (Integer, Integer) -- to keep signatures more readable
cTupleCompare :: CompiledCode (TupleInt -> TupleInt -> Ordering)
cTupleCompare = $$(compile [|| Ord.compare ||])

cTupleLt :: CompiledCode (TupleInt -> TupleInt -> Bool)
cTupleLt = $$(compile [|| (Ord.<) ||])

cTupleLte :: CompiledCode (TupleInt -> TupleInt -> Bool)
cTupleLte = $$(compile [|| (Ord.<=) ||])

cTupleGt :: CompiledCode (TupleInt -> TupleInt -> Bool)
cTupleGt = $$(compile [|| (Ord.>) ||])

cTupleGte :: CompiledCode (TupleInt -> TupleInt -> Bool)
cTupleGte = $$(compile [|| (Ord.>=) ||])

cTupleMax :: CompiledCode (TupleInt -> TupleInt -> TupleInt)
cTupleMax = $$(compile [|| Ord.max ||])

cTupleMin :: CompiledCode (TupleInt -> TupleInt -> TupleInt)
cTupleMin = $$(compile [|| Ord.min ||])
