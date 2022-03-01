module Size.Sqrt (test) where

import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude
import PlutusTx.Ratio as Plutus
import PlutusTx.Sqrt qualified as Sqrt
import PlutusTx.TH (compile)
import PlutusTx.Test (fitsInto)
import Test.Tasty (TestTree, testGroup)

test ::TestTree
test =
  testGroup "Sqrt" [
    testGroup "Calculation" [
      fitsInto "rsqrt" sqrtRsqrt 487,
      fitsInto "isqrt" sqrtIsqrt 494
    ],
    testGroup "Construction" [
      fitsInto "Imaginary" consImaginary 5,
      fitsInto "Exactly" consExactly 8,
      fitsInto "Approximately" consApproximately 8
    ]
  ]


sqrtRsqrt :: CompiledCode (Rational -> Sqrt.Sqrt)
sqrtRsqrt = $$(compile [|| Sqrt.rsqrt ||])

sqrtIsqrt :: CompiledCode (Integer -> Sqrt.Sqrt)
sqrtIsqrt = $$(compile [|| Sqrt.isqrt ||])

consImaginary :: CompiledCode Sqrt.Sqrt
consImaginary = $$(compile [|| Sqrt.Imaginary ||])

consExactly :: CompiledCode (Integer -> Sqrt.Sqrt)
consExactly = $$(compile [|| Sqrt.Exactly ||])

consApproximately :: CompiledCode (Integer -> Sqrt.Sqrt)
consApproximately = $$(compile [|| Sqrt.Approximately ||])
