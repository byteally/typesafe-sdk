{-# LANGUAGE OverloadedStrings #-}

-- | Error bodies and their classification, using bodies recorded from the
-- live API where possible.
module TypeSafe.ErrorSpec (tests) where

import Data.Aeson (toJSON)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Network.HTTP.Types (Status, mkStatus, status401, status403, status422)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import TypeSafe.Error
import TypeSafe.Wire (HTTPValidationError (..), LocationSegment (..), ValidationError (..))

tests :: TestTree
tests =
  testGroup
    "Errors"
    [ testCase "status codes map to kinds" $
        map (apiErrorKindFor . code) [400, 401, 403, 404, 408, 422, 429, 500, 502, 529, 302]
          @?= [ BadRequest
              , Unauthorized
              , PermissionDenied
              , NotFound
              , UnexpectedStatus
              , UnprocessableEntity
              , RateLimited
              , InternalServerError
              , InternalServerError
              , Overloaded
              , UnexpectedStatus
              ]
    , testCase "a missing API key (recorded: HTTP 403)" $ do
        let e = apiErrorFromResponse "GET /v1/models" status403 [("x-typesafe-request-id", "req_01a0")] missingKeyBody
        apiErrorKind e @?= PermissionDenied
        apiErrorMessage e @?= Just "Must supply an API key! Check your request and try again."
        apiErrorRequestId e @?= Just "req_01a0"
        apiErrorBody e
          @?= BodyDetail (ErrorDetail (Just "authentication_error") (Just "Must supply an API key! Check your request and try again."))
    , testCase "an invalid API key (recorded: HTTP 401)" $
        apiErrorKind (apiErrorFromResponse "GET /v1/models" status401 [] invalidKeyBody) @?= Unauthorized
    , testCase "validation errors keep every field" $ do
        let e = apiErrorFromResponse "POST /v1/systemone" status422 [] validationBody
        apiErrorBody e
          @?= BodyValidation
            ( HTTPValidationError
                ( Just
                    [ ValidationError
                        [LocationField "body", LocationField "questions", LocationField "urgency", LocationField "criteria", LocationIndex 0]
                        "Input should be a valid string"
                        "string_type"
                        (Just "3")
                        Nothing
                    ]
                )
            )
        apiErrorMessage e @?= Just "body.questions.urgency.criteria.0: Input should be a valid string"
    , testCase "bodies that are not structured errors" $ do
        parseErrorBody "" @?= BodyEmpty
        parseErrorBody "  \n" @?= BodyEmpty
        parseErrorBody "<html>Bad gateway</html>" @?= BodyText "<html>Bad gateway</html>"
        parseErrorBody "[1,2]" @?= BodyJson (toJSON [1, 2 :: Int])
    , testCase "rendered errors are one line and name the request" $ do
        let rendered =
              renderTypeSafeError
                (ServiceError (apiErrorFromResponse "GET /v1/models" status401 [("x-typesafe-request-id", "req_9")] invalidKeyBody))
        rendered
          @?= "GET /v1/models failed with HTTP 401 (Unauthorized): Cannot authenticate with the server. \
              \Please check your API key and try again. [request id req_9]"
        assertBool "single line" (not (Text.any (== '\n') rendered))
    ]
  where
    code :: Int -> Status
    code c = mkStatus c ""

missingKeyBody, invalidKeyBody, validationBody :: LBS.ByteString
missingKeyBody = "{\"detail\":{\"error_type\":\"authentication_error\",\"message\":\"Must supply an API key! Check your request and try again.\"}}"
invalidKeyBody = "{\"detail\":{\"error_type\":\"authentication_error\",\"message\":\"Cannot authenticate with the server. Please check your API key and try again.\"}}"
validationBody =
  "{\"detail\":[{\"type\":\"string_type\",\"loc\":[\"body\",\"questions\",\"urgency\",\"criteria\",0],\
  \\"msg\":\"Input should be a valid string\",\"input\":\"3\"}]}"
