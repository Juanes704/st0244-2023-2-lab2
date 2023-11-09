module RandomTests ( tests ) where

import Distribution.TestSuite.QuickCheck
import qualified Lib
import Test.QuickCheck

size :: Int
size = 3

b :: Char
b = 'b'

tests :: IO [ Test ]
tests = return 
    [ testProperty "Lib.valid respects boundaries" prop_validBoundaries,
      testProperty "Lib.valid checks the content" prop_validContent
    ]

prop_validBoundaries :: Lib.Grid -> Int -> Property
prop_validBoundaries g i = i >= 0 && i < (size^2) ==> Lib.valid g i

prop_validContent :: Lib.Grid -> Int -> Property
prop_validContent g i =
  i >= 0 && i < (size^2) && concat g !! i /= b ==>
  not (Lib.valid g i)
