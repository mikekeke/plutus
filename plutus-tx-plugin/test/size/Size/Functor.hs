module Size.Functor (test) where

import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Functor qualified as Functor
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import PlutusTx.Test (fitsInto)
import Test.Tasty (TestTree, testGroup)

test :: TestTree
test =
  testGroup "Functor" [
    testGroup "fmap" [
      fitsInto "List" cFampList 67,
      fitsInto "Maybe" cFampMaybe 25,
      fitsInto "Either" cFampEither 25,
      fitsInto "Tuple" cFampTuple 19,
      fitsInto "Identity" cFampIdentity 5
      -- fitsInto "Const" cFampConst 0 -- !PLC error  Unsupported feature: Kind: forall k. * -> k -> *
      ],
    fitsInto "<$>" cFampOp 5,
    fitsInto "<$" cReplaceOp 3
  ]

type Fmap f = (Integer -> Integer) -> f Integer -> f Integer

cFampList :: CompiledCode (Fmap [])
cFampList = $$(compile [|| Functor.fmap ||] )

cFampMaybe :: CompiledCode (Fmap Maybe)
cFampMaybe = $$(compile [|| Functor.fmap ||] )

cFampEither :: CompiledCode (Fmap (Either Integer))
cFampEither = $$(compile [|| Functor.fmap ||] )

cFampTuple :: CompiledCode (Fmap ((,) Integer))
cFampTuple = $$(compile [|| Functor.fmap ||] )

cFampIdentity :: CompiledCode (Fmap Identity)
cFampIdentity = $$(compile [|| Functor.fmap ||] )

cFampConst :: CompiledCode (Fmap (Const Integer))
cFampConst = $$(compile [|| Functor.fmap ||] )

cFampOp :: CompiledCode (Fmap Identity)
cFampOp = $$(compile [|| (Functor.<$>) ||] )

cReplaceOp :: CompiledCode (() -> Identity Integer -> Identity ())
cReplaceOp = $$(compile [|| (Functor.<$) ||] )
