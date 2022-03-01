module Size.Either (test) where

import PlutusTx.Code (CompiledCode)
import PlutusTx.Either qualified as Either
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import PlutusTx.Test (fitsInto)
import Test.Tasty (TestTree, testGroup)

test :: TestTree
test =
  testGroup "Either" [
    testGroup "Operations" [
      fitsInto "either" cEither 15,
      fitsInto "cIsRight" cIsRight 15,
      fitsInto "cIsLeft" cIsLeft 15
    ],
    testGroup "Construction" [
      fitsInto "Right" cRight 7,
      fitsInto "Left" cLeft 7
    ]
  ]

cEither :: CompiledCode ((Integer -> Integer) -> (Integer -> Integer) -> Either Integer Integer -> Integer)
cEither = $$(compile [|| Either.either ||])

cIsRight :: CompiledCode  (Either Integer Integer -> Bool)
cIsRight = $$(compile [|| Either.isRight ||])

cIsLeft :: CompiledCode  (Either Integer Integer -> Bool)
cIsLeft = $$(compile [|| Either.isLeft ||])

cRight :: CompiledCode  (Integer -> Either Integer Integer)
cRight = $$(compile [|| Either.Left ||])

cLeft :: CompiledCode  (Integer -> Either Integer Integer)
cLeft = $$(compile [|| Either.Left ||])
