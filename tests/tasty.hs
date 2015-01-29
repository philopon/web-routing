import Test.Tasty
import qualified Tree

main :: IO ()
main = defaultMain $ testGroup ""
    [ Tree.test
    ]
