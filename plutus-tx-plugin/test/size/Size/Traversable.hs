module Size.Traversable (test) where

import Data.Functor.Const (Const (Const))
import Data.Functor.Identity (Identity)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import PlutusTx.Test (fitsInto)
import PlutusTx.Traversable qualified as Traversable
import Test.Tasty (TestTree, testGroup)

test :: TestTree
test =
  testGroup "Traversable" [
    testGroup "traverse" [
      fitsInto "List" cTraverseList 177,
      fitsInto "Maybe" cTraverseMaybe 113,
      fitsInto "Either" cTraverseEither 120,
      fitsInto "Tuple" cTraverseTuple 36,
      fitsInto "Identity" cTraverseIdentity 25 --,
      -- fitsInto "Const" cTraverseConst 0 -- ! GHC Core to PLC plugin: E042:Error: Unsupported feature: Kind: forall k. * -> k -> *
      ],
    fitsInto "sequenceA" cSequenceA 336,
    fitsInto "sequence" cSequence 336,
    fitsInto "mapM" cMapM 333,
    fitsInto "for" cFor 339,
    fitsInto "fmapDefault" cFmapDefault 282 -- ,
    -- fitsInto "foldMapDefault" cFoldMapDefault 0 -- ! GHC Core to PLC plugin: E042:Error: Unsupported feature: Kind: forall k. * -> k -> *
                                                   -- ! uses `Const` in implementation
  ]

type Traverse t = (Integer -> Maybe Integer) -> t Integer -> Maybe (t Integer)

cTraverseList :: CompiledCode (Traverse [])
cTraverseList = $$(compile [|| Traversable.traverse ||])

cTraverseMaybe :: CompiledCode (Traverse Maybe)
cTraverseMaybe = $$(compile [|| Traversable.traverse ||])

cTraverseEither :: CompiledCode (Traverse (Either Integer))
cTraverseEither = $$(compile [|| Traversable.traverse ||])

cTraverseTuple :: CompiledCode (Traverse ((,) Integer))
cTraverseTuple = $$(compile [|| Traversable.traverse ||])

cTraverseIdentity :: CompiledCode (Traverse Identity)
cTraverseIdentity = $$(compile [|| Traversable.traverse ||])

cTraverseConst :: CompiledCode (Traverse (Const Integer))
cTraverseConst = $$(compile [|| (Traversable.traverse :: Traverse (Const Integer)) ||])


cSequenceA :: CompiledCode ([Maybe Integer] -> Maybe [Integer])
cSequenceA = $$(compile [|| Traversable.sequenceA ||])

cSequence :: CompiledCode ([Maybe Integer] -> Maybe [Integer])
cSequence = $$(compile [|| Traversable.sequence ||])

cMapM :: CompiledCode ((Integer -> Maybe Integer) -> [Integer] -> Maybe [Integer])
cMapM = $$(compile [|| Traversable.mapM ||])

cFor :: CompiledCode ([Integer] -> (Integer -> Maybe Integer) -> Maybe [Integer])
cFor = $$(compile [|| Traversable.for ||])

cFmapDefault :: CompiledCode ((Integer -> Integer) -> [Integer] -> [Integer])
cFmapDefault = $$(compile [|| Traversable.fmapDefault ||])

cFoldMapDefault :: CompiledCode ((Integer -> [Integer]) -> [Integer] -> [Integer])
cFoldMapDefault = $$(compile [|| Traversable.foldMapDefault ||])
