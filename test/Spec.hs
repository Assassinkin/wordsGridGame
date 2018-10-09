import Test.Hspec
import Lib
import Data

main :: IO ()
main = hspec $ do
  -- describe "How to write a test" $ do
  --   it "Should be able to run tests" $ do
  --     someString `shouldBe` "someString"

  describe "formatGrid" $ do
    it "Should concatenate every line with a newline" $ do
      (formatGrid ["abc", "def", "ghi"]) `shouldBe` "abc\ndef\nghi\n"

  describe "findWord" $ do
    it "Should find words that exist on the Grid" $ do
      findWord grid "HASKELL" `shouldBe` Just "HASKELL"
      findWord grid "PERL" `shouldBe` Just "PERL"
    it "Should not find words that do not exist on the grid" $ do
      findWord grid "DOVAH" `shouldBe` Nothing

  describe "FindWords" $ do
    it "Should find all words that exist on the Grid" $ do
      findWords grid languages `shouldBe` languages
    it "Should not find words that do not exist on the Grid" $ do
      findWords grid ["DOVAH", "KIN", "REM", "LOVEYA"] `shouldBe` []

-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented"
