{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generators for the wire types. They only produce values the
-- specification allows, so every generated value must validate against it.
module TypeSafe.ArbitraryInstances
  ( genText
  , genContent
  , genJsonValue
  , genProbability
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (nub, sort)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Test.QuickCheck
import TypeSafe.Content (Content (..))
import TypeSafe.Wire

genText :: Gen Text
genText =
  Text.pack
    <$> frequency
      [ (4, listOf (elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> " _-.`?")))
      , (1, getPrintableString <$> arbitrary)
      , (1, listOf (elements "äöü€日本語🙂\"\\\n\t"))
      ]

genKey :: Gen Text
genKey = Text.pack <$> listOf1 (elements (['a' .. 'z'] <> "_"))

-- | Finite probabilities, so that JSON round trips are exact.
genProbability :: Gen Double
genProbability = elements [0, 0.05, 0.1, 0.12, 0.25, 0.5, 0.75, 0.81, 0.88, 0.95, 1]

genJsonValue :: Int -> Gen Value
genJsonValue depth =
  frequency $
    [ (3, String <$> genText)
    , (2, Number . fromIntegral <$> (arbitrary :: Gen Int))
    , (1, Bool <$> arbitrary)
    , (1, pure Null)
    ]
      <> [ (1, Object . KeyMap.fromList <$> smallListOf ((,) <$> (Key.fromText <$> genKey) <*> genJsonValue (depth - 1)))
         | depth > 0
         ]
      <> [ (1, Array . Vector.fromList <$> smallListOf (genJsonValue (depth - 1)))
         | depth > 0
         ]

smallListOf :: Gen a -> Gen [a]
smallListOf g = choose (0, 3) >>= \n -> vectorOf n g

genContent :: Gen Content
genContent =
  frequency
    [ (4, ContentText <$> genText)
    , (1, ContentObject . KeyMap.fromList <$> smallListOf ((,) <$> (Key.fromText <$> genKey) <*> genJsonValue 2))
    , (1, ContentArray . Vector.fromList <$> smallListOf (genJsonValue 2))
    ]

-- | Distinct keys in sorted order, so that decoding an object reproduces the
-- same list.
genSortedKeys :: Gen [Text]
genSortedKeys = sort . nub <$> listOf1 genKey

instance Arbitrary Content where
  arbitrary = genContent

instance Arbitrary ModelName where
  arbitrary = ModelName <$> genText

instance Arbitrary QuestionId where
  arbitrary = QuestionId <$> genText

instance Arbitrary SystemOneRequest where
  arbitrary =
    SystemOneRequest
      <$> arbitrary
      <*> arbitrary
      <*> (Map.fromList <$> listOf1 ((,) <$> arbitrary <*> arbitrary))

instance Arbitrary Question where
  arbitrary =
    oneof
      [ QuestionNoul <$> arbitrary
      , QuestionChoice <$> arbitrary
      , QuestionScore <$> arbitrary
      ]

instance Arbitrary NoulQuestion where
  arbitrary = NoulQuestion <$> arbitrary <*> arbitrary

instance Arbitrary NoulCriteria where
  arbitrary = NoulCriteria <$> arbitrary <*> arbitrary

instance Arbitrary ChoiceQuestion where
  arbitrary = do
    keys <- genSortedKeys
    ChoiceQuestion
      <$> arbitrary
      <*> traverse (\k -> (,) k <$> arbitrary) keys

instance Arbitrary ScoreQuestion where
  arbitrary =
    ScoreQuestion
      <$> arbitrary
      <*> (NonEmpty.fromList <$> (choose (1, 10) >>= \n -> vectorOf n arbitrary))

instance Arbitrary SystemOneResponse where
  arbitrary =
    SystemOneResponse
      <$> arbitrary
      <*> (Map.fromList <$> listOf1 ((,) <$> arbitrary <*> arbitrary))
      <*> arbitrary

instance Arbitrary Answer where
  arbitrary =
    oneof
      [ AnswerNoul <$> arbitrary
      , AnswerChoice <$> arbitrary
      , AnswerScore <$> arbitrary
      ]

instance Arbitrary NoulAnswer where
  arbitrary = NoulAnswer <$> genProbability

instance Arbitrary ChoiceAnswer where
  arbitrary = do
    keys <- genSortedKeys
    ChoiceAnswer
      <$> elements keys
      <*> genProbability
      <*> (Map.fromList <$> traverse (\k -> (,) k <$> genProbability) keys)

instance Arbitrary ScoreAnswer where
  arbitrary = do
    levels <- choose (1, 10)
    ScoreAnswer
      <$> (fromIntegral <$> choose (0, levels - 1 :: Int))
      <*> genProbability
      <*> (Map.fromList <$> traverse (\i -> (,) i <$> arbitrary) [0 .. levels - 1])
      <*> (Map.fromList <$> traverse (\i -> (,) i <$> genProbability) [0 .. levels - 1])

instance Arbitrary Usage where
  arbitrary = Usage <$> (getNonNegative <$> arbitrary) <*> (getNonNegative <$> arbitrary)

instance Arbitrary ModelMetadataList where
  arbitrary = ModelMetadataList <$> smallListOf arbitrary

instance Arbitrary ModelMetadata where
  arbitrary =
    ModelMetadata
      <$> arbitrary
      <*> genText
      <*> elements ["2026-09-15", "2026-01-31", "not a date"]

instance Arbitrary HTTPValidationError where
  arbitrary = HTTPValidationError <$> liftArbitrary (smallListOf arbitrary)

instance Arbitrary ValidationError where
  arbitrary =
    ValidationError
      <$> smallListOf arbitrary
      <*> genText
      <*> genText
      <*> liftArbitrary (genJsonValue 2 `suchThat` (/= Null))
      <*> liftArbitrary (KeyMap.fromList <$> smallListOf ((,) <$> (Key.fromText <$> genKey) <*> genJsonValue 1))

instance Arbitrary LocationSegment where
  arbitrary = oneof [LocationField <$> genText, LocationIndex . getNonNegative <$> arbitrary]
