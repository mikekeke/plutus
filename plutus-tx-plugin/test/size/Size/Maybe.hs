module Size.Maybe (test) where

import PlutusTx.Code (CompiledCode)
import PlutusTx.Maybe qualified as Maybe
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import PlutusTx.Test (fitsInto)
import Test.Tasty (TestTree, testGroup)

test :: TestTree
test =
  testGroup "Maybe" [
    testGroup "Operations" [
      fitsInto "isJust" cIsJust 17,
      fitsInto "isNothing" cIsNothing 17,
      fitsInto "maybe" cMaybe 15,
      fitsInto "fromMaybe" cFromMaybe 12,
      fitsInto "mapMaybe" cMapMaybe 84
    ],
    testGroup "Construction" [
      fitsInto "Just" cJust 7,
      fitsInto "Nothing" cNothing 4
    ]
  ]


cIsJust :: CompiledCode (Maybe Integer -> Bool)
cIsJust = $$(compile [|| Maybe.isJust ||])

cIsNothing :: CompiledCode (Maybe Integer -> Bool)
cIsNothing = $$(compile [|| Maybe.isNothing ||])

cMaybe ::  CompiledCode (Integer -> (Integer -> Integer) -> Maybe Integer -> Integer)
cMaybe = $$(compile [|| Maybe.maybe ||])

cFromMaybe ::  CompiledCode(Integer -> Maybe Integer -> Integer)
cFromMaybe = $$(compile [|| Maybe.fromMaybe ||])

cMapMaybe ::  CompiledCode((Integer -> Maybe Integer) -> [Integer] -> [Integer])
cMapMaybe = $$(compile [|| Maybe.mapMaybe ||])

cJust ::  CompiledCode(Integer -> Maybe Integer)
cJust = $$(compile [|| Maybe.Just ||])

cNothing ::  CompiledCode( Maybe Integer)
cNothing = $$(compile [|| Maybe.Nothing ||])
