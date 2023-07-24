import DataHelpers
import DataTests
import Set1
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Supporting libraries" $ do
    DataTests.tests
    DataHelpers.tests

  describe "Cryptopals" $ do
    Set1.tests
