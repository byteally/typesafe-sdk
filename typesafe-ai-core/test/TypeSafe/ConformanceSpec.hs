{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Checks the bindings in "TypeSafe.Wire" and "TypeSafe.Call" against the
-- vendored OpenAPI specification (@spec/openapi.json@).
--
-- When TypeSafe publishes a new version of the specification, replace the
-- vendored file (see @scripts/sync-spec.sh@) and run these tests: each
-- failure names an endpoint, schema, property or keyword that the bindings
-- do not cover yet.
module TypeSafe.ConformanceSpec (tests) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, ToJSON (..), Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Either (isLeft)
import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Test.QuickCheck (Arbitrary, Gen, arbitrary, generate, vectorOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (counterexample, testProperty, (===))
import TypeSafe.ArbitraryInstances ()
import TypeSafe.Call (callEndpoint, listModels, systemOne, systemOneRaw)
import TypeSafe.JsonSchema
import TypeSafe.Question (ask, noul)
import TypeSafe.Wire

-- | A Haskell type bound to a schema of the specification.
data Binding = forall a. (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a) => Binding Text (Proxy a)

-- | Every schema the SDK binds. A schema added to the specification makes the
-- \"every schema is bound\" test fail until it is listed here.
bindings :: [Binding]
bindings =
  [ Binding "Answer" (Proxy :: Proxy Answer)
  , Binding "ChoiceAnswer" (Proxy :: Proxy ChoiceAnswer)
  , Binding "ChoiceQuestion" (Proxy :: Proxy ChoiceQuestion)
  , Binding "HTTPValidationError" (Proxy :: Proxy HTTPValidationError)
  , Binding "ModelMetadata" (Proxy :: Proxy ModelMetadata)
  , Binding "ModelMetadataList" (Proxy :: Proxy ModelMetadataList)
  , Binding "NoulAnswer" (Proxy :: Proxy NoulAnswer)
  , Binding "NoulCriteria" (Proxy :: Proxy NoulCriteria)
  , Binding "NoulQuestion" (Proxy :: Proxy NoulQuestion)
  , Binding "Question" (Proxy :: Proxy Question)
  , Binding "ScoreAnswer" (Proxy :: Proxy ScoreAnswer)
  , Binding "ScoreQuestion" (Proxy :: Proxy ScoreQuestion)
  , Binding "SystemOneRequest" (Proxy :: Proxy SystemOneRequest)
  , Binding "SystemOneResponse" (Proxy :: Proxy SystemOneResponse)
  , Binding "Usage" (Proxy :: Proxy Usage)
  , Binding "ValidationError" (Proxy :: Proxy ValidationError)
  ]

-- | Every operation the SDK can call, as @(METHOD, path)@.
sdkOperations :: [(Text, Text)]
sdkOperations =
  nub
    [ (method, path)
    | endpoint <-
        [ callEndpoint (systemOne "state" (ask "q" (noul "?")))
        , callEndpoint (systemOneRaw (SystemOneRequest "state" jevLatest mempty))
        , callEndpoint listModels
        ]
    , let (method, path) = fmap Text.strip (Text.breakOn " " endpoint)
    ]

tests :: Spec -> TestTree
tests spec =
  testGroup
    "OpenAPI conformance"
    [ testCase "apiSpecVersion matches the vendored specification" $
        Just apiSpecVersion @?= specVersion spec
    , testCase "every operation is bound" $
        sort sdkOperations @?= sort (specOperations spec)
    , testCase "every schema is bound" $
        sort [name | Binding name _ <- bindings] @?= sort (specSchemaNames spec)
    , testCase "the schema validator understands every keyword in use" $
        nub (filter (`notElem` supportedKeywords) (specKeywords spec)) @?= []
    , testCase "question constructors match the Question discriminator" $
        sort
          [ questionType (QuestionNoul (NoulQuestion Nothing Nothing))
          , questionType (QuestionChoice (ChoiceQuestion Nothing []))
          , questionType (QuestionScore (ScoreQuestion Nothing ("level" :| [])))
          ]
          @?= sort (discriminatorTags spec "Question")
    , testCase "answer constructors match the Answer discriminator" $
        sort
          [ answerType (AnswerNoul (NoulAnswer 0))
          , answerType (AnswerChoice (ChoiceAnswer "a" 0 mempty))
          , answerType (AnswerScore (ScoreAnswer 0 0 mempty mempty))
          ]
          @?= sort (discriminatorTags spec "Answer")
    , testCase "unknown question and answer types pass through unchanged" $ do
        let question = Aeson.object ["type" Aeson..= ("rank" :: Text), "items" Aeson..= [1 :: Int, 2]]
        fmap toJSON (Aeson.eitherDecode (Aeson.encode question) :: Either String Question) @?= Right question
        let answer = Aeson.object ["type" Aeson..= ("rank" :: Text), "order" Aeson..= [2 :: Int, 1]]
        fmap toJSON (Aeson.eitherDecode (Aeson.encode answer) :: Either String Answer) @?= Right answer
    , testGroup "schemas" (map (schemaTests spec) bindings)
    ]

schemaTests :: Spec -> Binding -> TestTree
schemaTests spec (Binding name (_ :: Proxy a)) =
  testGroup (Text.unpack name) $
    [ testProperty "encoded values validate against the schema" $ \(x :: a) ->
        let errors = validate spec (schemaRef name) (toJSON x)
         in counterexample (unlines errors) (null errors)
    , testProperty "toEncoding agrees with toJSON" $ \(x :: a) ->
        Aeson.decode (Aeson.encode x) === Just (toJSON x)
    , testProperty "decoding inverts encoding" $ \(x :: a) ->
        Aeson.eitherDecode (Aeson.encode x) === Right x
    ]
      <> [ testCase "encodes exactly the declared properties" $ do
             samples <- generate (vectorOf 500 (arbitrary :: Gen a))
             let emitted = Set.unions [Set.fromList (objectKeys (toJSON x)) | x <- samples]
             emitted @?= Set.fromList properties
         | not (null properties)
         ]
      <> [ testCase "requires the required properties and only those" $ do
             samples <- generate (vectorOf 50 (arbitrary :: Gen a))
             forM_ samples $ \x -> forM_ (objectKeys (toJSON x)) $ \p -> do
               let decoded = decodeValue (withoutKey p (toJSON x))
               if p `elem` required
                 then assertBool ("decoded without required property " <> show p) (isLeft decoded)
                 else case decoded of
                   Left err -> assertFailure ("optional property " <> show p <> " is required by the decoder: " <> err)
                   Right _ -> pure ()
         | not (null properties)
         ]
      <> [ testCase "the specification's examples validate and decode" $ do
             let example = Object (KeyMap.fromList [(Key.fromText p, v) | (p, v) <- examples])
             validate spec (schemaRef name) example @?= []
             case decodeValue example of
               Left err -> assertFailure ("cannot decode " <> show example <> ": " <> err)
               Right _ -> pure ()
         | not (null properties)
         , all (`elem` map fst examples) required
         ]
  where
    properties = objectProperties spec name
    required = requiredProperties spec name
    examples = propertyExamples spec name
    decodeValue :: Value -> Either String a
    decodeValue v = Aeson.eitherDecode (Aeson.encode v)

objectKeys :: Value -> [Text]
objectKeys (Object o) = map Key.toText (KeyMap.keys o)
objectKeys _ = []

withoutKey :: Text -> Value -> Value
withoutKey k (Object o) = Object (KeyMap.delete (Key.fromText k) o)
withoutKey _ v = v
