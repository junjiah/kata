module Huffman.Test where

import           Data.Maybe (fromJust)
import           Huffman
import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "basic tests" $ let fs = frequencies "aaaabcc" in do
        it "aaaabcc encoded should have length 10" $
            fmap length (encode fs "aaaabcc") `shouldBe` Just 10
        it "empty list encode" $
            encode fs [] `shouldBe` Just []
        it "empty list decode" $
            decode fs [] `shouldBe` Just []
        it "simple encode / decode" $
            decode fs (fromJust $ encode fs "abc") `shouldBe` Just "abc"

    describe "error handling" $ do
        it "empty frequencies encode 1" $ encode [] "abc" `shouldBe` Nothing
        it "empty frequencies encode 2" $ encode [] "" `shouldBe` Nothing
        it "singleton frequency encode 1" $ encode [('a', 1)] "a" `shouldBe` Nothing
        it "singleton frequency encode 2" $ encode [('a', 1)] "" `shouldBe` Nothing

        it "empty frequencies decode 1" $ (decode [] [Z, O] :: Maybe String) `shouldBe` Nothing
        it "empty frequencies decode 2" $ (decode [] [] :: Maybe String) `shouldBe` Nothing
        it "singleton frequency decode 1" $ decode [('a', 1)] [Z, O] `shouldBe` Nothing
        it "singleton frequency decode 2" $ decode [('a', 1)] [] `shouldBe` Nothing
