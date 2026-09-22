{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A small validator for the subset of JSON Schema that FastAPI emits in
-- OpenAPI 3.1 documents.
--
-- Unknown keywords are reported as errors rather than ignored. When a new
-- version of the TypeSafe specification starts using a keyword this
-- validator does not understand, the conformance tests fail and point at it,
-- instead of silently passing.
module TypeSafe.JsonSchema
  ( Spec
  , loadSpec
  , specVersion
  , specOperations
  , specSchemaNames
  , schemaRef
  , lookupSchema
  , objectProperties
  , requiredProperties
  , discriminatorTags
  , propertyExamples
  , specKeywords
  , supportedKeywords
  , validate
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- | A parsed OpenAPI document.
newtype Spec = Spec Value

loadSpec :: FilePath -> IO Spec
loadSpec path =
  Aeson.eitherDecodeFileStrict path >>= \case
    Left err -> fail ("cannot parse " <> path <> ": " <> err)
    Right v -> pure (Spec v)

at :: [Text] -> Value -> Maybe Value
at [] v = Just v
at (k : ks) (Object o) = KeyMap.lookup (Key.fromText k) o >>= at ks
at _ _ = Nothing

keysOf :: Value -> [Text]
keysOf (Object o) = map Key.toText (KeyMap.keys o)
keysOf _ = []

-- | @info.version@.
specVersion :: Spec -> Maybe Text
specVersion (Spec v) = case at ["info", "version"] v of
  Just (String t) -> Just t
  _ -> Nothing

-- | Every operation, as @(METHOD, path)@.
specOperations :: Spec -> [(Text, Text)]
specOperations (Spec v) =
  [ (Text.toUpper method, path)
  | path <- keysOf (fromMaybe Null (at ["paths"] v))
  , method <- keysOf (fromMaybe Null (at ["paths", path] v))
  ]

-- | The names under @components.schemas@.
specSchemaNames :: Spec -> [Text]
specSchemaNames (Spec v) = keysOf (fromMaybe Null (at ["components", "schemas"] v))

-- | A schema that refers to a named component.
schemaRef :: Text -> Value
schemaRef name = Aeson.object ["$ref" Aeson..= ("#/components/schemas/" <> name)]

lookupSchema :: Spec -> Text -> Maybe Value
lookupSchema (Spec v) name = at ["components", "schemas", name] v

-- | The declared properties of an object schema.
objectProperties :: Spec -> Text -> [Text]
objectProperties spec name = keysOf (fromMaybe Null (lookupSchema spec name >>= at ["properties"]))

-- | The required properties of an object schema.
requiredProperties :: Spec -> Text -> [Text]
requiredProperties spec name = case lookupSchema spec name >>= at ["required"] of
  Just (Array xs) -> [t | String t <- toList xs]
  _ -> []

-- | The tags of a discriminated union, from its @discriminator.mapping@.
discriminatorTags :: Spec -> Text -> [Text]
discriminatorTags spec name = keysOf (fromMaybe Null (lookupSchema spec name >>= at ["discriminator", "mapping"]))

-- | The first example of every property of an object schema that has one.
propertyExamples :: Spec -> Text -> [(Text, Value)]
propertyExamples spec name =
  mapMaybe
    ( \p -> case lookupSchema spec name >>= at ["properties", p, "examples"] of
        Just (Array xs) | not (Vector.null xs) -> Just (p, Vector.head xs)
        _ -> Nothing
    )
    (objectProperties spec name)

-- | Every JSON Schema keyword used anywhere under @components.schemas@.
specKeywords :: Spec -> [Text]
specKeywords (Spec v) = case at ["components", "schemas"] v of
  Just (Object schemas) -> concatMap schemaKeys (KeyMap.elems schemas)
  _ -> []
  where
    schemaKeys = \case
      Object s -> map Key.toText (KeyMap.keys s) <> concatMap (uncurry nested) (KeyMap.toList s)
      _ -> []
    nested k sub = case (Key.toText k, sub) of
      ("properties", Object props) -> concatMap schemaKeys (KeyMap.elems props)
      ("additionalProperties", Object _) -> schemaKeys sub
      ("items", Object _) -> schemaKeys sub
      ("anyOf", Array alts) -> concatMap schemaKeys (toList alts)
      ("oneOf", Array alts) -> concatMap schemaKeys (toList alts)
      _ -> []

-- | The keywords 'validate' understands.
supportedKeywords :: [Text]
supportedKeywords =
  [ "$ref", "type", "const", "properties", "required", "additionalProperties", "items"
  , "minItems", "minProperties", "anyOf", "oneOf"
  , "discriminator", "title", "description", "examples", "default"
  ]

-- | Validate a JSON value against a schema. Returns one message per problem,
-- each prefixed with a JSON path; an empty list means the value is valid.
validate :: Spec -> Value -> Value -> [String]
validate spec = go "$"
  where
    go :: String -> Value -> Value -> [String]
    go path schema value = case schema of
      Bool True -> []
      Bool False -> [path <> ": no value is allowed here"]
      Object s -> concatMap (keyword path s value) (KeyMap.toList s)
      _ -> [path <> ": malformed schema " <> show schema]

    keyword :: String -> Aeson.Object -> Value -> (Key.Key, Value) -> [String]
    keyword path s value (k, arg) = case Key.toText k of
      "$ref" -> case arg of
        String ref
          | Just name <- Text.stripPrefix "#/components/schemas/" ref
          , Just target <- lookupSchema spec name ->
              go path target value
        _ -> [path <> ": cannot resolve $ref " <> show arg]
      "type" -> case arg of
        String t -> [path <> ": expected " <> Text.unpack t <> ", got " <> kind value | not (hasType t value)]
        Array ts ->
          [ path <> ": expected one of " <> show (toList ts) <> ", got " <> kind value
          | not (or [hasType t value | String t <- toList ts])
          ]
        _ -> [path <> ": malformed type"]
      "const" -> [path <> ": expected " <> show arg <> ", got " <> show value | arg /= value]
      "properties" -> case (arg, value) of
        (Object props, Object o) ->
          concat
            [ go (path <> "." <> Key.toString p) propSchema v
            | (p, propSchema) <- KeyMap.toList props
            , Just v <- [KeyMap.lookup p o]
            ]
        _ -> []
      "required" -> case (arg, value) of
        (Array names, Object o) ->
          [ path <> ": missing required property " <> Text.unpack n
          | String n <- toList names
          , not (KeyMap.member (Key.fromText n) o)
          ]
        _ -> []
      "additionalProperties" -> case value of
        Object o ->
          let declared = case KeyMap.lookup "properties" s of
                Just (Object props) -> props
                _ -> KeyMap.empty
           in concat
                [ go (path <> "." <> Key.toString p) arg v
                | (p, v) <- KeyMap.toList o
                , not (KeyMap.member p declared)
                ]
        _ -> []
      "items" -> case value of
        Array xs -> concat [go (path <> "[" <> show i <> "]") arg x | (i, x) <- zip [0 :: Int ..] (toList xs)]
        _ -> []
      "minItems" -> case (arg, value) of
        (Number n, Array xs) -> [path <> ": fewer than " <> show n <> " items" | fromIntegral (length xs) < n]
        _ -> []
      "minProperties" -> case (arg, value) of
        (Number n, Object o) -> [path <> ": fewer than " <> show n <> " properties" | fromIntegral (KeyMap.size o) < n]
        _ -> []
      "anyOf" -> case arg of
        Array alternatives
          | any null [go path alt value | alt <- toList alternatives] -> []
          | otherwise -> [path <> ": matches none of the anyOf alternatives"]
        _ -> [path <> ": malformed anyOf"]
      "oneOf" -> case arg of
        Array alternatives ->
          let matching = length (filter null [go path alt value | alt <- toList alternatives])
           in [path <> ": matches " <> show matching <> " oneOf alternatives, expected exactly 1" | matching /= 1]
        _ -> [path <> ": malformed oneOf"]
      -- Annotations that do not constrain values.
      "discriminator" -> []
      "title" -> []
      "description" -> []
      "examples" -> []
      "default" -> []
      other -> [path <> ": unsupported JSON Schema keyword " <> show other <> "; teach TypeSafe.JsonSchema about it"]

    hasType :: Text -> Value -> Bool
    hasType t v = case (t, v) of
      ("string", String _) -> True
      ("number", Number _) -> True
      ("integer", Number n) -> Scientific.isInteger n
      ("boolean", Bool _) -> True
      ("object", Object _) -> True
      ("array", Array _) -> True
      ("null", Null) -> True
      _ -> False

    kind :: Value -> String
    kind = \case
      String _ -> "a string"
      Number _ -> "a number"
      Bool _ -> "a boolean"
      Object _ -> "an object"
      Array _ -> "an array"
      Null -> "null"
