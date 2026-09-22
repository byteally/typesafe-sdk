{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TypeSafe.Content
-- Description : Text or structured JSON content
-- Copyright   : (c) 2026 byteally
-- License     : BSD-3-Clause
--
-- The TypeSafe API accepts /content/ in several places: the @state@ being
-- evaluated, a question's @instructions@, the descriptions of Choice options,
-- Score levels and Noul criteria. Everywhere, content is either a plain string
-- or structured JSON (an object or an array). Numbers, booleans and @null@ are
-- not valid content on their own.
--
-- 'Content' models exactly that. With @OverloadedStrings@ a string literal is
-- 'Content':
--
-- >>> encode ("Help! My payouts have been failing for 3 days." :: Content)
-- "\"Help! My payouts have been failing for 3 days.\""
--
-- Structured content is usually built with 'contentObject' or, for any type
-- with a 'ToJSON' instance, 'contentJSON':
--
-- >>> encode (contentObject ["subject" .= ("Duplicate charge" :: Text), "priority" .= (2 :: Int)])
-- "{\"priority\":2,\"subject\":\"Duplicate charge\"}"
--
-- See <https://docs.typesafe.ai/concepts/state> for advice on structuring
-- state, and <https://docs.typesafe.ai/primitives/advanced> for structured
-- instructions and criteria.
module TypeSafe.Content
  ( Content (..)
  , contentText
  , contentObject
  , contentArray
  , contentJSON
  , contentFromValue
  , contentToValue
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson
  ( FromJSON (..)
  , Object
  , ToJSON (..)
  , Value (..)
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Array, Pair)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as Vector
import GHC.Generics (Generic)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Aeson (encode, (.=), toJSON)
-- >>> import Data.Text (Text)

-- | Text, or a structured JSON object or array.
--
-- Use 'ContentText' (or a string literal) for prose, and 'ContentObject' or
-- 'ContentArray' for records, chat logs and other structured data. Questions
-- can point at fields of structured state by path, for example
-- @\"Does \`ticket.messages[0].text\` request a refund?\"@.
data Content
  = -- | A plain string.
    ContentText !Text
  | -- | A JSON object.
    ContentObject !Object
  | -- | A JSON array.
    ContentArray !Array
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | String literals are 'ContentText'.
instance IsString Content where
  fromString = ContentText . Text.pack

instance ToJSON Content where
  toJSON = contentToValue
  toEncoding = \case
    ContentText t -> toEncoding t
    ContentObject o -> toEncoding o
    ContentArray a -> toEncoding a

-- | Accepts a string, object or array; rejects numbers, booleans and @null@.
instance FromJSON Content where
  parseJSON v = case contentFromValue v of
    Just c -> pure c
    Nothing -> fail ("expected a string, an object or an array, but got " <> describe v)
    where
      describe = \case
        Number _ -> "a number"
        Bool _ -> "a boolean"
        Null -> "null"
        _ -> "an unsupported value"

-- | Plain text content.
contentText :: Text -> Content
contentText = ContentText

-- | Build an object from key/value pairs, like 'Data.Aeson.object'.
--
-- >>> encode (contentObject ["message" .= ("Please help." :: Text)])
-- "{\"message\":\"Please help.\"}"
contentObject :: [Pair] -> Content
contentObject = ContentObject . KeyMap.fromList

-- | Build an array of JSON values.
--
-- >>> encode (contentArray [toJSON ("first" :: Text), toJSON ("second" :: Text)])
-- "[\"first\",\"second\"]"
contentArray :: [Value] -> Content
contentArray = ContentArray . Vector.fromList

-- | Content from any value with a 'ToJSON' instance.
--
-- Objects, arrays and strings map to the matching constructor. A scalar
-- (number, boolean or @null@) is not valid content on its own, so it is sent
-- as its JSON text instead.
--
-- >>> contentJSON (["a", "b"] :: [Text])
-- ContentArray [String "a",String "b"]
--
-- >>> contentJSON (42 :: Int)
-- ContentText "42"
contentJSON :: (ToJSON a) => a -> Content
contentJSON a = case toJSON a of
  String t -> ContentText t
  Object o -> ContentObject o
  Array xs -> ContentArray xs
  scalar -> ContentText (Text.decodeUtf8 (LBS.toStrict (Aeson.encode scalar)))

-- | Convert a JSON value, if it is a string, object or array.
--
-- >>> contentFromValue (toJSON True)
-- Nothing
contentFromValue :: Value -> Maybe Content
contentFromValue = \case
  String t -> Just (ContentText t)
  Object o -> Just (ContentObject o)
  Array xs -> Just (ContentArray xs)
  _ -> Nothing

-- | The JSON value sent on the wire.
contentToValue :: Content -> Value
contentToValue = \case
  ContentText t -> String t
  ContentObject o -> Object o
  ContentArray xs -> Array xs
