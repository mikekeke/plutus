module Main (main) where

import Prelude
import Size.Applicative qualified as SizeApplicative
import Size.AssocMap qualified as SizeAssocMap
import Size.Bool qualified as SizeBool
import Size.Either qualified as SizeEither
import Size.Enum qualified as SizeEnum
import Size.Eq qualified as SizeEq
import Size.List qualified as SizeList
import Size.Maybe qualified as SizeMaybe
import Size.Ord qualified as SizeOrd
import Size.Ratio qualified as SizeRatio
import Size.Sqrt qualified as SizeSqrt
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain . testGroup "Size regression tests" $ [
    SizeSqrt.test
  , SizeBool.test
  , SizeEither.test
  , SizeEnum.test
  , SizeEq.test
  , SizeOrd.test
  , SizeRatio.test
  , SizeMaybe.test
  , SizeAssocMap.test
  , SizeList.test
  , SizeApplicative.test
  ]
