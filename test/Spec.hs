import Test.Hspec
import Lib
import Data


gwc = gridWithCoords grid

testFindWord word =
  let (Just result) = findWord gwc word
      string = map cell2char result
  in string `shouldBe` word

main :: IO ()
main = hspec $ do
  -- describe "How to write a test" $ do
  --   it "Should be able to run tests" $ do
  --     someString `shouldBe` "someString"

      describe "formatGrid" $ do
        it "Should concatenate every line with a newline" $ do
          (formatGrid (gridWithCoords ["abc", "def", "ghi"])) `shouldBe` "abc\ndef\nghi\n"

      describe "findWord" $ do
        it "Should find words that exist on the Grid" $ do
          testFindWord "HASKELL"
          testFindWord "PERL"
        it "Should not find words that do not exist on the Grid" $ do
          findWord gwc "DOVAH" `shouldBe` Nothing

      describe "findWords" $ do
        it "Should find all the words that exist on the Grid" $ do
          let found = findWords gwc languages
              asString = map (map cell2char) found
          asString `shouldBe` languages
        it "Should not find words that do not exist on the Grid" $ do
          findWords gwc ["DOVAH", "KIN", "REM", "LOVEYA"] `shouldBe` []
-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented"
