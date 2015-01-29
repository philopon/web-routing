module Tree(test) where

import Network.Routing.Tree
import Test.Tasty
import Test.Tasty.QuickCheck
import Unsafe.Coerce

mkTestTree :: Int -> Tree
mkTestTree i = foldr cons Tip $ map unsafeCoerce [0..i]

testTreeToList :: Int -> Tree -> [Int]
testTreeToList l t = foldr (\i b -> unsafeCoerce (t `index` i) : b) [] [0..l]

test :: TestTree
test = testProperty "Data.Apiary.Tree" $ \len ->
    testTreeToList len (mkTestTree len) == [0..len]
