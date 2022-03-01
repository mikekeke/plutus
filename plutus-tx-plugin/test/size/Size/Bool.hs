{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

module Size.Bool (test) where

import PlutusTx.Bool qualified as Bool
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import PlutusTx.Test (fitsInto)
import Test.Tasty (TestTree, testGroup)

test ::TestTree
test =
  testGroup "Bool" [
    testGroup "Operations" [
      fitsInto "&&" cAnd 14,
      fitsInto "||" cOr 14,
      fitsInto "not" cNot 16,
      fitsInto "otherwise" cOtherwise 4
    ],
    testGroup "Construction" [
      fitsInto "True" cTrue 4,
      fitsInto "False" cFalse 4
    ]
  ]

cAnd :: CompiledCode (Bool -> Bool -> Bool)
cAnd = $$(compile [|| (Bool.&&) ||])

cOr :: CompiledCode (Bool -> Bool -> Bool)
cOr = $$(compile [|| (Bool.||) ||])

cNot :: CompiledCode (Bool -> Bool)
cNot = $$(compile [|| Bool.not ||])

cOtherwise :: CompiledCode Bool
cOtherwise = $$(compile [|| Bool.otherwise ||])

cTrue :: CompiledCode Bool
cTrue = $$(compile [|| Bool.True ||])

cFalse :: CompiledCode Bool
cFalse = $$(compile [|| Bool.False ||])
