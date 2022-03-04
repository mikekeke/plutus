module Size.Applicative (test) where

import Control.Applicative (Const)
import Data.Functor.Identity (Identity)
import PlutusTx.Applicative qualified as Applicative
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import PlutusTx.Test (fitsInto)
import Test.Tasty (TestTree, testGroup)

test :: TestTree
test =
  testGroup "Applicative" [
    fitsInto "cLiftA2" cLiftA2 101,
    fitsInto "cStarArrR" cStarArrR 105,
    fitsInto "cStarArrL" cStarArrL 102,
    fitsInto "cUnless" cUnless 18,
    testGroup "Maybe" [
      fitsInto "pure" cPureMaybe 7,
      fitsInto "<*>" cAppMaybe 40
      ],
    testGroup "Either" [
      fitsInto "pure" cPureEither 7,
      fitsInto "<*>" cAppEither 46
      ],
    testGroup "Identity" [
      fitsInto "pure" cPureIdentity 2,
      fitsInto "<*>" cAppIdentity 5
      ],
    testGroup "Const" [
      fitsInto "pure" cPureConst 5,
      fitsInto "<*>" cAppConst 73
      ]
  ]

cLiftA2 :: CompiledCode ((Integer -> Integer -> Integer) ->
                          Maybe Integer ->
                          Maybe Integer ->
                          Maybe Integer
                        )
cLiftA2 = $$(compile [|| Applicative.liftA2 ||])

cStarArrR :: CompiledCode (Maybe Integer -> Maybe Integer ->  Maybe Integer)
cStarArrR = $$(compile [|| (Applicative.*>) ||])

cStarArrL :: CompiledCode (Maybe Integer -> Maybe Integer ->  Maybe Integer)
cStarArrL = $$(compile [|| (Applicative.<*) ||])

cUnless :: CompiledCode (Bool -> Maybe () ->  Maybe ())
cUnless = $$(compile [|| Applicative.unless ||])

-- Maybe

cPureMaybe :: CompiledCode (Integer -> Maybe Integer)
cPureMaybe = $$(compile [|| Applicative.pure ||])

cAppMaybe :: CompiledCode (Maybe (Integer -> Integer) -> Maybe Integer -> Maybe Integer)
cAppMaybe = $$(compile [|| (Applicative.<*>) ||])

-- Either

cPureEither :: CompiledCode (Integer -> Either Integer Integer)
cPureEither = $$(compile [|| Applicative.pure ||])

cAppEither :: CompiledCode (Either Integer (Integer -> Integer) ->
                            Either Integer  Integer ->
                            Either Integer  Integer)
cAppEither = $$(compile [|| (Applicative.<*>) ||])

-- Identity

cPureIdentity :: CompiledCode (Integer -> Identity Integer)
cPureIdentity = $$(compile [|| Applicative.pure ||])

cAppIdentity :: CompiledCode (Identity (Integer -> Integer) ->
                              Identity Integer ->
                              Identity Integer)
cAppIdentity = $$(compile [|| (Applicative.<*>) ||])

-- Const

cPureConst :: CompiledCode (Integer -> Const [Integer] Integer)
cPureConst = $$(compile [|| Applicative.pure ||])

cAppConst :: CompiledCode (Const [Integer] (Integer -> Integer) ->
                            Const [Integer]  Integer ->
                            Const [Integer]  Integer)
cAppConst = $$(compile [|| (Applicative.<*>) ||])
