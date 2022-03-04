module Size.Foldable (test) where

import Data.Functor.Const (Const (Const))
import Data.Functor.Identity (Identity)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Foldable qualified as Foldable
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import PlutusTx.Test (fitsInto)
import Test.Tasty (TestTree, testGroup)

test :: TestTree
test =
  testGroup "Foldable" [
    testGroup "foldMap" [
      fitsInto "[]" cFoldMapList 158,
      fitsInto "Maybe" cFoldMapMaybe 66,
      fitsInto "Either" cFoldMapEither 64,
      fitsInto "Tuple" cFoldMapTuple 59,
      fitsInto "Identity" cFoldMapIdentity 54
      -- fitsInto "" cFoldMapConst 0 -- !PLC error
      ],
    testGroup "Special biased folds" [
      -- fitsInto "foldrM" cFoldrM 0, -- !PLC error compiling GHC.Base.map
      -- fitsInto "foldrM" cFoldrM 0 -- !PLC error compiling GHC.Base.map
      ],
    testGroup "Other" [
      fitsInto "fold" cFold 164,
      fitsInto "foldr" cFoldr 93,
      fitsInto "foldl" cFoldl 146,
      fitsInto "toList" cToList 25,
      fitsInto "null" cNull 99,
      fitsInto "length" cLength 144,
      fitsInto "elem" cElem 141,
      fitsInto "sum" cSum 113,
      fitsInto "product" cProduct 113,
      fitsInto "traverse_" cTraverse_ 209,
      fitsInto "sequence_" cSequence_ 320,
      fitsInto "for_" cFor_ 209,
      fitsInto "sequenceA_" cSequenceA_ 206,
      fitsInto "asum" cAsum 461,
      fitsInto "concat" cConcat 135,
      fitsInto "concatMap" cConcatMap 138,
      fitsInto "and" cAnd 123,
      fitsInto "or" cOr 123,
      fitsInto "any" cAny 123,
      fitsInto "all" cAll 123,
      fitsInto "notElem" cNotElem 152,
      fitsInto "find" cFind 121,
      fitsInto "mapM_" cMapM_ 323
      ]
  ]

type FoldMap t = (Integer -> [Integer]) -> t Integer -> [Integer]

cFoldMapList :: CompiledCode (FoldMap [])
cFoldMapList = $$(compile [|| Foldable.foldMap ||])

cFoldMapMaybe :: CompiledCode (FoldMap Maybe)
cFoldMapMaybe = $$(compile [|| Foldable.foldMap ||])

cFoldMapEither :: CompiledCode (FoldMap (Either Integer))
cFoldMapEither = $$(compile [|| Foldable.foldMap ||])

cFoldMapTuple :: CompiledCode (FoldMap ((,) Integer))
cFoldMapTuple = $$(compile [|| Foldable.foldMap ||])

cFoldMapIdentity :: CompiledCode (FoldMap Identity)
cFoldMapIdentity = $$(compile [|| Foldable.foldMap ||])

cFoldMapConst :: CompiledCode ((Integer -> [Integer]) -> Const () Integer -> [Integer])
cFoldMapConst = $$(compile [|| Foldable.foldMap ||])

cFold :: CompiledCode ([[Integer]] -> [Integer])
cFold = $$(compile [|| Foldable.fold ||])

cFoldr :: CompiledCode ((Integer -> Integer -> Integer) ->
                        Integer ->
                        [Integer] ->
                        Integer
                       )
cFoldr = $$(compile [|| Foldable.foldr ||])

cFoldl :: CompiledCode ((Integer -> Integer -> Integer) ->
                        Integer ->
                        [Integer] ->
                        Integer
                       )
cFoldl = $$(compile [|| Foldable.foldl ||])

cToList :: CompiledCode (Maybe Integer -> [Integer])
cToList = $$(compile [|| Foldable.toList ||])

cNull :: CompiledCode ([Integer] -> Bool)
cNull = $$(compile [|| Foldable.null ||])

cLength :: CompiledCode ([Integer] -> Integer)
cLength = $$(compile [|| Foldable.length ||])

cElem :: CompiledCode (Integer -> [Integer] -> Bool)
cElem = $$(compile [|| Foldable.elem ||])

cSum :: CompiledCode ([Integer] -> Integer)
cSum = $$(compile [|| Foldable.sum ||])

cProduct :: CompiledCode ([Integer] -> Integer)
cProduct = $$(compile [|| Foldable.product ||])

cTraverse_ :: CompiledCode ((Integer -> Maybe Integer) -> [Integer] -> Maybe ())
cTraverse_ = $$(compile [|| Foldable.traverse_ ||])

-- cSequence_ :: CompiledCode (Maybe [Integer] -> [()]) -- !PLC error compiling GHC.Base.map
cSequence_ :: CompiledCode ([Maybe Integer] -> Maybe ())
cSequence_ = $$(compile [|| Foldable.sequence_ ||])

cFor_ :: CompiledCode ([Integer] -> (Integer -> Maybe Integer) -> Maybe ())
cFor_ = $$(compile [|| Foldable.for_ ||])

cSequenceA_ :: CompiledCode ([Maybe Integer] -> Maybe ())
cSequenceA_ = $$(compile [|| Foldable.sequenceA_ ||])


cAsum :: CompiledCode ([Maybe Integer] -> Maybe Integer )
cAsum = $$(compile [|| Foldable.asum ||])

cConcat :: CompiledCode (Maybe [Integer] ->  [Integer])
cConcat = $$(compile [|| Foldable.concat ||])

cConcatMap :: CompiledCode ((Integer -> [Integer]) -> Maybe Integer -> [Integer])
cConcatMap = $$(compile [|| Foldable.concatMap ||])

cAnd :: CompiledCode ([Bool] -> Bool)
cAnd = $$(compile [|| Foldable.and ||])

cOr :: CompiledCode ([Bool] -> Bool)
cOr = $$(compile [|| Foldable.or ||])

cAny :: CompiledCode ((Integer -> Bool) -> [Integer] -> Bool)
cAny = $$(compile [|| Foldable.any ||])

cAll :: CompiledCode ((Integer -> Bool) -> [Integer] -> Bool)
cAll = $$(compile [|| Foldable.all ||])

cNotElem :: CompiledCode (Integer -> [Integer] -> Bool)
cNotElem = $$(compile [|| Foldable.notElem ||])

cFind :: CompiledCode ((Integer -> Bool) -> [Integer] -> Maybe Integer)
cFind = $$(compile [|| Foldable.find ||])


cMapM_ :: CompiledCode ((Integer -> Maybe Integer) -> [Integer] -> Maybe ())
cMapM_ = $$(compile [|| Foldable.mapM_ ||])

cFoldrM :: CompiledCode ((Integer -> Integer -> [Integer]) ->
                         Integer ->
                         Maybe Integer ->
                         [Integer]
                        )
cFoldrM = $$(compile [|| Foldable.foldrM ||])

cFoldlM :: CompiledCode ((Integer -> Integer -> [Integer]) ->
                         Integer ->
                         Maybe Integer ->
                         [Integer]
                        )
cFoldlM = $$(compile [|| Foldable.foldlM ||])
