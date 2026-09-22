-- |
-- Module      : TypeSafe.Core
-- Description : Types, codecs and calls for the TypeSafe API, without HTTP
-- Copyright   : (c) 2026 byteally
-- License     : BSD-3-Clause
--
-- Everything needed to talk to the TypeSafe System One API
-- (<https://docs.typesafe.ai>) except the network. Most applications should
-- depend on the @typesafe-ai@ package instead, which re-exports this module
-- together with a ready-to-use client.
--
-- Depend on @typesafe-ai-core@ directly when you want to bring your own HTTP
-- stack: this package depends only on @aeson@, @http-types@ and GHC's boot
-- libraries.
--
-- = Layers
--
-- ["TypeSafe.Question"] typed questions and answers: Choice options and
-- Score levels are your own Haskell types.
--
-- ["TypeSafe.Call"] API calls as values, plus the functions a transport needs
-- to send them.
--
-- ["TypeSafe.Wire"] the OpenAPI schemas, one Haskell type per schema, with
-- JSON codecs. Not re-exported here; import it (qualified) when you need it.
--
-- ["TypeSafe.Error"], ["TypeSafe.Retry"] errors and the retry policy.
--
-- = With servant or another HTTP library
--
-- The wire types have 'Data.Aeson.ToJSON' and 'Data.Aeson.FromJSON'
-- instances, so describing the API in another framework takes a few lines.
-- With servant, for example:
--
-- @
-- import qualified TypeSafe.Wire as Wire
--
-- type TypeSafeAPI =
--   Header' '[Required, Strict] \"Authorization\" Text
--     :> \"v1\"
--     :> ( \"systemone\" :> ReqBody '[JSON] Wire.SystemOneRequest :> Post '[JSON] Wire.SystemOneResponse
--            :\<|\> \"models\" :> Get '[JSON] Wire.ModelMetadataList
--        )
-- @
--
-- The typed layer works with any such client: build the request with
-- 'systemOneRequest' and decode the response with 'decodeEvaluation'.
--
-- @
-- case 'systemOneRequest' 'jevLatest' state triage of
--   Left err -> …                                  -- rejected locally
--   Right request -> do
--     response <- runClientM (systemOneClient auth request) env
--     … 'decodeEvaluation' triage Nothing response …
-- @
--
-- To reuse the retry policy, error classification and header handling of the
-- bundled client as well, write a transport instead: see "TypeSafe.Call".
module TypeSafe.Core
  ( -- * Content
    module TypeSafe.Content

    -- * Questions and answers
  , module TypeSafe.Question

    -- * Calls
  , module TypeSafe.Call

    -- * Errors
  , module TypeSafe.Error

    -- * Retries
  , module TypeSafe.Retry

    -- * Identifiers and response metadata
  , ModelName (..)
  , jevLatest
  , jevPreview
  , QuestionId (..)
  , Usage (..)
  , ModelMetadata (..)
  , modelMetadataReleaseDay
  , apiSpecVersion
  ) where

import TypeSafe.Call
import TypeSafe.Content
import TypeSafe.Error
import TypeSafe.Question
import TypeSafe.Retry
import TypeSafe.Wire
  ( ModelMetadata (..)
  , ModelName (..)
  , QuestionId (..)
  , Usage (..)
  , apiSpecVersion
  , jevLatest
  , jevPreview
  , modelMetadataReleaseDay
  )
