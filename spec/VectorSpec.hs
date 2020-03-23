module VectorSpec (spec) where

import Test.Hspec
import Vector

spec :: Spec
spec = do
  describe "Functor" $ do
    it "fmap applies the function on each Vector coordinates" $ do
      fmap (+2) (Vector 1 2 3) `shouldBe` (Vector 3 4 5)

  describe "Semigroup" $ do
    it "(<>) appends two Vectors" $ do
      (Vector 1 2 3) <> (Vector 4 5 6) `shouldBe` (Vector 5 7 9)

  describe "Monoid" $ do
    it "mempty returns 'zero' Vector" $ do
      mempty `shouldBe` (Vector 0 0 0)

  it "adds Vectors" $ do
    (Vector 1 2 3) |+| (Vector 4 5 6) `shouldBe` (Vector 5 7 9)

  it "substracts Vectors" $ do
    (Vector 6 5 4) |-| (Vector 4 5 6) `shouldBe` (Vector 2 0 (-2))

  it "multiplies Vectors by scalar" $ do
    (Vector 6 5 4) |* 4 `shouldBe` (Vector 24 20 16)
    4 *| (Vector 6 5 4) `shouldBe` (Vector 24 20 16)

  it "divides Vectors by scalar" $ do
    (Vector 24 20 16) |/ 4 `shouldBe` (Vector 6 5 4)

  it "returns Vectors dot product" $ do
    dot (Vector 1 2 3) (Vector 4 5 6) `shouldBe` 32
    dot (Vector 1 0 0) (Vector 0 0 1) `shouldBe` 0
    dot (Vector 1 0 0) (Vector 0 1 1) `shouldBe` 0

  it "returns Vectors cross product" $ do
    cross (Vector 1 2 3) (Vector 4 5 6) `shouldBe` (Vector (-3) 6 (-3))
    cross (Vector 1 0 0) (Vector 0 0 1) `shouldBe` (Vector 0 (-1) 0)

  it "returns Vectors length" $ do
    magnitude (Vector 3 4 0) `shouldBe` 5

  it "returns distance between Vectors" $ do
    distance (Vector 3 4 0) (Vector 1 2 3) `shouldBe` magnitude (Vector 2 2 (-3))

  it "returns normalized Vector" $ do
    magnitude (norm (Vector 3 4 0)) `shouldBe` 1
    norm (Vector 3 4 0) `shouldBe` (Vector 0.6 0.8 0.0)
