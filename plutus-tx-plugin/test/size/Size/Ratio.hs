module Size.Ratio (test) where

import PlutusTx.Code (CompiledCode)
import PlutusTx.IsData.Class (fromBuiltinData, toBuiltinData, unsafeFromBuiltinData)
import PlutusTx.Prelude
import PlutusTx.Ratio qualified as PlutusRatio
import PlutusTx.TH (compile)
import PlutusTx.Test (fitsInto, fitsUnder)
import Test.Tasty (TestTree, testGroup)

test ::TestTree
test = testGroup "Rational" [
    testGroup "Eq" [
      fitsInto "==" ratEq 55,
      fitsInto "/=" ratNeq 63
      ],
    testGroup "Ord" [
      fitsInto "compare" ratCompare 89,
      fitsInto "<=" ratLe 38,
      fitsInto ">=" ratGe 38,
      fitsInto "<" ratLt 38,
      fitsInto ">" ratGt 38,
      fitsInto "max" ratMax 46,
      fitsInto "min" ratMin 46
      ],
    testGroup "Additive" [
      fitsInto "+" ratPlus 134,
      fitsInto "zero" ratZero 7,
      fitsInto "-" ratMinus 134,
      fitsInto "negate (specialized)" ratNegate 20
      ],
    testGroup "Multiplicative" [
      fitsInto "*" ratTimes 126,
      fitsInto "one" ratOne 10,
      fitsInto "scale" ratScale 100
      ],
    testGroup "Serialization" [
      fitsInto "toBuiltinData" ratToBuiltin 33,
      fitsInto "fromBuiltinData" ratFromBuiltin 411,
      fitsInto "unsafeFromBuiltinData" ratUnsafeFromBuiltin 256
      ],
    testGroup "Construction" [
      fitsInto "unsafeRatio" ratMkUnsafe 168,
      fitsInto "ratio" ratMkSafe 264,
      fitsInto "fromInteger" ratFromInteger 8
      ],
    testGroup "Other" [
      fitsInto "numerator" ratNumerator 7,
      fitsInto "denominator" ratDenominator 7,
      fitsInto "round" ratRound 332,
      fitsInto "truncate" ratTruncate 11,
      fitsInto "properFraction" ratProperFraction 36,
      fitsInto "recip" ratRecip 87,
      fitsInto "abs (specialized)" ratAbs 58
      ],
    testGroup "Comparison" [
      fitsUnder "negate" ("specialized", ratNegate) ("general", genNegate),
      fitsUnder "abs" ("specialized", ratAbs) ("general", genAbs),
      fitsUnder "scale" ("type class method", ratScale) ("equivalent in other primitives", genScale)
      ]
    ]


-- Compiled definitions

ratEq :: CompiledCode (Rational -> Rational -> Bool)
ratEq = $$(compile [|| (==) ||])

ratNeq :: CompiledCode (Rational -> Rational -> Bool)
ratNeq = $$(compile [|| (/=) ||])

ratCompare :: CompiledCode (Rational -> Rational -> Ordering)
ratCompare = $$(compile [|| compare ||])

ratLe :: CompiledCode (Rational -> Rational -> Bool)
ratLe = $$(compile [|| (<=) ||])

ratLt :: CompiledCode (Rational -> Rational -> Bool)
ratLt = $$(compile [|| (<) ||])

ratGe :: CompiledCode (Rational -> Rational -> Bool)
ratGe = $$(compile [|| (>=) ||])

ratGt :: CompiledCode (Rational -> Rational -> Bool)
ratGt = $$(compile [|| (>) ||])

ratMax :: CompiledCode (Rational -> Rational -> Rational)
ratMax = $$(compile [|| max ||])

ratMin :: CompiledCode (Rational -> Rational -> Rational)
ratMin = $$(compile [|| min ||])

ratPlus :: CompiledCode (Rational -> Rational -> Rational)
ratPlus = $$(compile [|| (+) ||])

ratZero :: CompiledCode Rational
ratZero = $$(compile [|| zero ||])

ratMinus :: CompiledCode (Rational -> Rational -> Rational)
ratMinus = $$(compile [|| (-) ||])

ratNegate :: CompiledCode (Rational -> Rational)
ratNegate = $$(compile [|| PlutusRatio.negate ||])

ratTimes :: CompiledCode (Rational -> Rational -> Rational)
ratTimes = $$(compile [|| (*) ||])

ratOne :: CompiledCode Rational
ratOne = $$(compile [|| one ||])

ratScale :: CompiledCode (Integer -> Rational -> Rational)
ratScale = $$(compile [|| scale ||])

ratToBuiltin :: CompiledCode (Rational -> BuiltinData)
ratToBuiltin = $$(compile [|| toBuiltinData ||])

ratFromBuiltin :: CompiledCode (BuiltinData -> Maybe Rational)
ratFromBuiltin = $$(compile [|| fromBuiltinData ||])

ratUnsafeFromBuiltin :: CompiledCode (BuiltinData -> Rational)
ratUnsafeFromBuiltin = $$(compile [|| unsafeFromBuiltinData ||])

ratMkUnsafe :: CompiledCode (Integer -> Integer -> Rational)
ratMkUnsafe = $$(compile [|| PlutusRatio.unsafeRatio ||])

ratMkSafe :: CompiledCode (Integer -> Integer -> Maybe Rational)
ratMkSafe = $$(compile [|| PlutusRatio.ratio ||])

ratNumerator :: CompiledCode (Rational -> Integer)
ratNumerator = $$(compile [|| PlutusRatio.numerator ||])

ratDenominator :: CompiledCode (Rational -> Integer)
ratDenominator = $$(compile [|| PlutusRatio.denominator ||])

ratRound :: CompiledCode (Rational -> Integer)
ratRound = $$(compile [|| PlutusRatio.round ||])

ratTruncate :: CompiledCode (Rational -> Integer)
ratTruncate = $$(compile [|| PlutusRatio.truncate ||])

ratProperFraction :: CompiledCode (Rational -> (Integer, Rational))
ratProperFraction = $$(compile [|| PlutusRatio.properFraction ||])

ratRecip :: CompiledCode (Rational -> Rational)
ratRecip = $$(compile [|| PlutusRatio.recip ||])

ratAbs :: CompiledCode (Rational -> Rational)
ratAbs = $$(compile [|| PlutusRatio.abs ||])

ratFromInteger :: CompiledCode (Integer -> Rational)
ratFromInteger = $$(compile [|| PlutusRatio.fromInteger ||])

genNegate :: CompiledCode (Rational -> Rational)
genNegate = $$(compile [|| negate ||])

genAbs :: CompiledCode (Rational -> Rational)
genAbs = $$(compile [|| abs ||])

genScale :: CompiledCode (Integer -> Rational -> Rational)
genScale = $$(compile [|| \s v -> PlutusRatio.fromInteger s * v ||])
