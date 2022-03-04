module Size.AssocMap (test) where

import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Code (CompiledCode)
import PlutusTx.IsData.Class (fromBuiltinData, toBuiltinData, unsafeFromBuiltinData)
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import PlutusTx.Test (fitsInto)
import PlutusTx.These (These)
import Test.Tasty (TestTree, testGroup)

test :: TestTree
test =
  testGroup "AssocMap" [
    testGroup "Eq" [
      fitsInto "==" cEq 157,
      fitsInto "==" cNeq 171
      ],
    testGroup "Ord" [
      fitsInto "compare" cCompare 619,
      fitsInto "<" cLt 636,
      fitsInto "<=" cLte 636,
      fitsInto ">" cGt 636,
      fitsInto ">=" cGte 636,
      fitsInto "min" cMax 636,
      fitsInto "max" cMin 636
      ],
    testGroup "Serialization" [
      fitsInto "toBuiltinData" cToBuiltin 74,
      fitsInto "fromBuiltinData" cFromBuiltinData 225,
      fitsInto "unsafeFromBuiltinData" cUnsafeFromBuiltinData 92
      ],
    testGroup "Functor" [
      fitsInto "fmap" cFmap 90
      ],
    testGroup "Foldable" [
      fitsInto "foldMap" cFoldMap 172
      ],
    testGroup "Traversable" [
      fitsInto "traverse" cTraverse 228
      ],
    testGroup "Semigroup" [
      fitsInto "<>" cMappend 493
      ],
    testGroup "Monoid" [
      fitsInto "mempty" cMemepty 4
      ],
    testGroup "Construction" [
      fitsInto "singleton" cSicngleton 19,
      fitsInto "empty" cEmpty 4
      ],
    testGroup "Other" [
      fitsInto "null" cNull 99,
      fitsInto "fromList" cFromList 2,
      fitsInto "toList" cToList 2,
      fitsInto "elems" cElems 78,
      fitsInto "lookup" cLookup 73,
      fitsInto "member" cMember 71,
      fitsInto "insert" cInsert 499,
      fitsInto "delete" cDelete 84,
      fitsInto "union" cUnion 444,
      fitsInto "unionWith" cUnionWith 482,
      fitsInto "filter" cFilter 91,
      fitsInto "mapWithKey" cMapWithKey 92,
      fitsInto "mapMaybe" cMapMaybe 98,
      fitsInto "mapMaybeWithKey" cMapMaybeWithKey 100,
      fitsInto "all" cAll 137,
      fitsInto "mapThese" cMapThese 338
     ]
  ]

type MapInt = -- to keep signatures short
  AssocMap.Map Integer Integer

cEq :: CompiledCode (MapInt -> MapInt -> Bool)
cEq = $$(compile [|| (==) ||])

cNeq :: CompiledCode (MapInt -> MapInt -> Bool)
cNeq = $$(compile [|| (/=) ||])


cCompare :: CompiledCode (MapInt -> MapInt -> Ordering )
cCompare = $$(compile [|| compare ||])

cLt :: CompiledCode (MapInt -> MapInt -> Bool)
cLt = $$(compile [|| (<) ||])

cLte :: CompiledCode (MapInt -> MapInt -> Bool)
cLte = $$(compile [|| (<=) ||])

cGt :: CompiledCode (MapInt -> MapInt -> Bool)
cGt = $$(compile [|| (>) ||])

cGte :: CompiledCode (MapInt -> MapInt -> Bool)
cGte = $$(compile [|| (>=) ||])

cMax :: CompiledCode (MapInt -> MapInt -> MapInt)
cMax = $$(compile [|| max ||])

cMin :: CompiledCode (MapInt -> MapInt -> MapInt)
cMin = $$(compile [|| min ||])

cToBuiltin :: CompiledCode (MapInt -> BuiltinData )
cToBuiltin = $$(compile  [|| toBuiltinData ||])

cFromBuiltinData :: CompiledCode (BuiltinData  -> Maybe MapInt)
cFromBuiltinData = $$(compile  [|| fromBuiltinData ||])

cUnsafeFromBuiltinData :: CompiledCode (BuiltinData  -> MapInt)
cUnsafeFromBuiltinData = $$(compile  [|| unsafeFromBuiltinData ||])

cFmap :: CompiledCode ((Integer -> Integer)  -> MapInt -> MapInt)
cFmap = $$(compile [|| fmap ||])

cFoldMap :: CompiledCode ((Integer -> [Integer]) -> MapInt -> [Integer])
cFoldMap = $$(compile [|| foldMap ||])


cTraverse :: CompiledCode ((Integer -> Maybe Integer) -> MapInt -> Maybe MapInt)
cTraverse = $$(compile [|| traverse ||])

type MapIntList = AssocMap.Map Integer [Integer]
cMappend :: CompiledCode (MapIntList -> MapIntList -> MapIntList)
cMappend = $$(compile [|| (<>) ||])

cMemepty :: CompiledCode MapIntList
cMemepty = $$(compile [|| mempty ||])

cSicngleton :: CompiledCode (Integer -> Integer -> MapInt)
cSicngleton = $$(compile [|| AssocMap.singleton ||])

cEmpty :: CompiledCode MapInt
cEmpty = $$(compile [|| AssocMap.empty ||])

cNull :: CompiledCode (MapInt -> Bool)
cNull = $$(compile [|| AssocMap.null ||])

cFromList :: CompiledCode ([(Integer, Integer)] -> MapInt)
cFromList =  $$(compile [|| AssocMap.fromList ||])

cToList :: CompiledCode (MapInt -> [(Integer, Integer )])
cToList =  $$(compile [|| AssocMap.toList ||])

cKeys :: CompiledCode (MapInt -> [Integer])
cKeys =  $$(compile [|| AssocMap.keys ||])

cElems :: CompiledCode (MapInt -> [Integer])
cElems =  $$(compile [|| AssocMap.elems ||])

cLookup :: CompiledCode (Integer -> MapInt -> Maybe Integer)
cLookup =  $$(compile [|| AssocMap.lookup ||])

cMember :: CompiledCode (Integer -> MapInt -> Bool)
cMember =  $$(compile [|| AssocMap.member ||])

cInsert :: CompiledCode (Integer -> Integer -> MapInt -> MapInt)
cInsert =  $$(compile [|| AssocMap.insert ||])

cDelete :: CompiledCode (Integer -> MapInt -> MapInt)
cDelete =  $$(compile [|| AssocMap.delete ||])

cUnion :: CompiledCode (MapInt -> MapInt -> AssocMap.Map Integer (These Integer Integer))
cUnion =  $$(compile [|| AssocMap.union ||])

cUnionWith :: CompiledCode ((Integer -> Integer -> Integer) -> MapInt -> MapInt -> MapInt)
cUnionWith =  $$(compile [|| AssocMap.unionWith ||])

cFilter :: CompiledCode ((Integer -> Bool) -> MapInt -> MapInt)
cFilter =  $$(compile [|| AssocMap.filter ||])

cMapWithKey :: CompiledCode ((Integer -> Integer -> Integer) -> MapInt -> MapInt )
cMapWithKey =  $$(compile [|| AssocMap.mapWithKey ||])

cMapMaybe :: CompiledCode ((Integer -> Maybe Integer) -> MapInt -> MapInt)
cMapMaybe =  $$(compile [|| AssocMap.mapMaybe ||])

cMapMaybeWithKey :: CompiledCode ((Integer -> Integer -> Maybe Integer) -> MapInt -> MapInt )
cMapMaybeWithKey =  $$(compile [|| AssocMap.mapMaybeWithKey ||])

cAll :: CompiledCode ((Integer -> Bool) -> MapInt -> Bool)
cAll =  $$(compile [|| AssocMap.all ||])

cMapThese :: CompiledCode ((Integer -> These Integer Integer) -> MapInt -> (MapInt, MapInt))
cMapThese =  $$(compile [|| AssocMap.mapThese ||])
