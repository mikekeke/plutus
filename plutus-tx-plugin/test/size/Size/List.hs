module Size.List (test) where

import PlutusTx.Code (CompiledCode)
import PlutusTx.List qualified as List
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import PlutusTx.Test (fitsInto)
import Test.Tasty (TestTree, testGroup)

test :: TestTree
test =
  testGroup "List" [
    fitsInto "map" cMap 67,
    fitsInto "filter" cFilter 83,
    fitsInto "listToMaybe" cListToMaybe 20,
    fitsInto "uniqueElement" cUniqueElement 36,
    fitsInto "findIndices" cFindIndices 70,
    fitsInto "findIndex" cFindIndex 91,
    fitsInto "foldr" cFoldr 56,
    fitsInto "reverse" cReverse 49,
    fitsInto "zip" cZip 84,
    fitsInto "(++)" cAppend 73,
    fitsInto "(!!)" cIndex 124,
    fitsInto "head" cHead 25,
    fitsInto "take" cTake 94,
    fitsInto "tail" cTail 25,
    fitsInto "nub" cNub 181,
    fitsInto "nubBy" cNubBy 163,
    fitsInto "zipWith" cZipWith 71,
    fitsInto "dropWhile" cDropWhile 59,
    fitsInto "partition" cPartition 159,
    fitsInto "sort" cSort 648,
    fitsInto "sortBy" cSortBy 582
  ]

cMap :: CompiledCode ((Integer -> Integer) -> [Integer] -> [Integer])
cMap = $$(compile [|| List.map ||])

cFilter :: CompiledCode ((Integer -> Bool) -> [Integer] -> [Integer])
cFilter = $$(compile [|| List.filter ||])

cListToMaybe :: CompiledCode ([Integer] -> Maybe Integer)
cListToMaybe = $$(compile [|| List.listToMaybe ||])

cUniqueElement :: CompiledCode ([Integer] -> Maybe Integer)
cUniqueElement = $$(compile [|| List.uniqueElement ||])

cFindIndices :: CompiledCode ((Integer -> Bool) -> [Integer] -> [Integer])
cFindIndices = $$(compile [|| List.findIndices ||])

cFindIndex :: CompiledCode ((Integer -> Bool) -> [Integer] -> Maybe Integer)
cFindIndex = $$(compile [|| List.findIndex ||])

cFoldr :: CompiledCode ((Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer)
cFoldr = $$(compile [|| List.foldr ||])

cReverse :: CompiledCode ([Integer] -> [Integer])
cReverse = $$(compile [|| List.reverse ||])

cZip :: CompiledCode ([Integer] -> [Integer] -> [(Integer, Integer)])
cZip = $$(compile [|| List.zip ||])

cAppend :: CompiledCode ([Integer] -> [Integer] -> [Integer])
cAppend = $$(compile [|| (List.++) ||])

cIndex :: CompiledCode ([Integer] -> Integer -> Integer)
cIndex = $$(compile [|| (List.!!) ||])

cHead :: CompiledCode ([Integer] -> Integer)
cHead = $$(compile [|| List.head ||])

cTake :: CompiledCode (Integer -> [Integer] -> [Integer])
cTake = $$(compile [|| List.take ||])

cTail :: CompiledCode ([Integer] -> [Integer])
cTail = $$(compile [|| List.tail ||])

cNub :: CompiledCode ([Integer] -> [Integer])
cNub = $$(compile [|| List.nub ||])

cNubBy :: CompiledCode ((Integer -> Integer -> Bool) -> [Integer] -> [Integer])
cNubBy = $$(compile [|| List.nubBy ||])

cZipWith :: CompiledCode ((Integer -> Integer -> Integer) -> [Integer] -> [Integer] -> [Integer])
cZipWith = $$(compile [|| List.zipWith ||])

cDropWhile :: CompiledCode ((Integer -> Bool) -> [Integer] -> [Integer])
cDropWhile = $$(compile [|| List.dropWhile ||])

cPartition :: CompiledCode ((Integer -> Bool) -> [Integer] -> ([Integer], [Integer]))
cPartition = $$(compile [|| List.partition ||])

cSort :: CompiledCode ([Integer] -> [Integer])
cSort = $$(compile [|| List.sort ||])

cSortBy :: CompiledCode ((Integer -> Integer -> Ordering) -> [Integer] -> [Integer])
cSortBy = $$(compile [|| List.sortBy ||])
