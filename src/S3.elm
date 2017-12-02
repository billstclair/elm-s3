----------------------------------------------------------------------
--
-- S3.elm
-- Elm client library for Amazon's S3 (Simple Storage Service)
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module S3
    exposing
        ( Request
        , addHeaders
        , addQuery
        , decodeAccounts
        , deleteObject
        , getFullObject
        , getObject
        , getObjectWithHeaders
        , htmlBody
        , jsonBody
        , listKeys
        , putHtmlObject
        , putObject
        , putPublicObject
        , readAccounts
        , send
        , stringBody
        )

{-| Elm client for the AWS Simple Storage Service (S3) or Digital Ocean Spaces.


# Types

@docs Request


# Functions

@docs readAccounts, decodeAccounts
@docs htmlBody, jsonBody, stringBody
@docs listKeys, getObject, getFullObject, getObjectWithHeaders, deleteObject
@docs putObject, putPublicObject, putHtmlObject
@docs addQuery, addHeaders
@docs send

-}

import AWS.Core.Credentials
    exposing
        ( Credentials
        , fromAccessKeys
        )
import AWS.Core.Http
    exposing
        ( Body
        , Method(..)
        , emptyBody
        , request
        )
import AWS.Core.Service as Service exposing (ApiVersion, Protocol, Service)
import Dict
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import S3.Parser
    exposing
        ( parseListBucketResponse
        )
import S3.Types
    exposing
        ( Account
        , Bucket
        , CannedAcl(..)
        , Error(..)
        , Key
        , KeyList
        , Mimetype
        , Query
        , QueryElement(..)
        , StorageClass
        , aclToString
        )
import Task exposing (Task)


defaultAccountsUrl : String
defaultAccountsUrl =
    "accounts.json"


{-| Read JSON from a URL and turn it into a list of `Account`s.

If `Nothing` is passed for the first arg (the URL), will use the default of `"accounts.json"`.

You're not going to want to store the secret keys in this JSON in plain text anywhere but your development machine. I'll add support eventually for encryption of the accounts JSON.

Example JSON (the `buckets` are used only by the example code):

    [{"name": "Digital Ocean",
      "region": "nyc3",
      "is-digital-ocean": true,
      "access-key": "<20-character access key>",
      "secret-key": "<40-character secret key>",
      "buckets": ["bucket1","bucket2"]
     },
     {"name": "Amazon S3",
      "region": "us-east-1",
      "access-key": "<20-character access key>",
      "secret-key": "<40-character secret key>",
      "buckets": ["bucket3","bucket4","bucket5"]
     }
    ]

-}
readAccounts : Maybe String -> Task Error (List Account)
readAccounts maybeUrl =
    let
        url =
            case maybeUrl of
                Just u ->
                    u

                Nothing ->
                    defaultAccountsUrl

        request =
            Http.getString url

        getTask =
            Http.toTask request
    in
    Task.andThen decodeAccountsTask <|
        Task.onError handleHttpError getTask


decodeAccountsTask : String -> Task Error (List Account)
decodeAccountsTask json =
    case decodeAccounts json of
        Ok accounts ->
            Task.succeed accounts

        Err error ->
            Task.fail error


handleHttpError : Http.Error -> Task Error String
handleHttpError error =
    Task.fail <| HttpError error


makeCredentials : Account -> Credentials
makeCredentials account =
    fromAccessKeys account.accessKey account.secretKey


serviceModifier : Bool -> Service -> Service
serviceModifier isDigitalOcean =
    if isDigitalOcean then
        Service.toDigitalOceanSpaces
    else
        identity


accountDecoder : Decoder Account
accountDecoder =
    JD.map6 Account
        (JD.field "name" JD.string)
        (JD.oneOf
            [ JD.field "region" (JD.nullable JD.string)
            , JD.succeed Nothing
            ]
        )
        (JD.field "access-key" JD.string)
        (JD.field "secret-key" JD.string)
        (JD.field "buckets" (JD.list JD.string))
        (JD.oneOf
            [ JD.field "is-digital-ocean" JD.bool
            , JD.succeed False
            ]
        )


accountsDecoder : Decoder (List Account)
accountsDecoder =
    JD.list accountDecoder


{-| Decode a JSON string encoding a list of `Account`s
-}
decodeAccounts : String -> Result Error (List Account)
decodeAccounts json =
    case JD.decodeString accountsDecoder json of
        Err s ->
            Err <| DecodeError s

        Ok accounts ->
            Ok accounts


endpointPrefix : String
endpointPrefix =
    "s3"


apiVersion : ApiVersion
apiVersion =
    "2017-07-10"


protocol : Protocol
protocol =
    Service.restXml


makeService : Account -> Service
makeService { region, isDigitalOcean } =
    let
        modifier =
            serviceModifier isDigitalOcean
    in
    case region of
        Nothing ->
            Service.defineGlobal
                endpointPrefix
                apiVersion
                protocol
                Service.signS3
                modifier

        Just region ->
            Service.defineRegional
                endpointPrefix
                apiVersion
                protocol
                Service.signS3
                modifier
                region


{-| A request that can be turned into a Task by `S3.send`.
-}
type Request b a
    = Request
        { httpRequest : AWS.Core.Http.Request b
        , andThen : b -> Task Error a
        }


identityAndThen : a -> Task Error a
identityAndThen res =
    Task.succeed res


{-| Create a `Task` to send a signed request over the wire.
-}
send : Account -> Request b a -> Task Error a
send account req =
    let
        service =
            makeService account

        credentials =
            makeCredentials account
    in
    case addHeaders [ AnyQuery "Accept" "*/*" ] req of
        Request { httpRequest, andThen } ->
            AWS.Core.Http.send service credentials httpRequest
                |> Task.onError (Task.fail << HttpError)
                |> Task.andThen andThen


formatQuery : Query -> List ( String, String )
formatQuery query =
    let
        formatElement =
            \element ->
                case element of
                    AnyQuery k v ->
                        ( k, v )

                    Delimiter s ->
                        ( "delimiter", s )

                    Marker s ->
                        ( "marker", s )

                    MaxKeys cnt ->
                        ( "max-keys", toString cnt )

                    Prefix s ->
                        ( "prefix", s )

                    XAmzAcl acl ->
                        ( "x-amz-acl", aclToString acl )
    in
    List.map formatElement query


{-| Add headers to a `Request`.
-}
addHeaders : Query -> Request b a -> Request b a
addHeaders headers request =
    case request of
        Request req ->
            Request
                { req
                    | httpRequest =
                        AWS.Core.Http.addHeaders
                            (formatQuery headers)
                            req.httpRequest
                }


{-| Add query parameters to a `Request`.
-}
addQuery : Query -> Request b a -> Request b a
addQuery query request =
    case request of
        Request req ->
            Request
                { req
                    | httpRequest =
                        AWS.Core.Http.addQuery (formatQuery query) req.httpRequest
                }


parserRequest : Method -> String -> Body -> (Http.Response String -> Result String b) -> (b -> Task Error a) -> Request b a
parserRequest method url body parser andThen =
    let
        req =
            request method url body (JD.fail "Can't happen.")
    in
    Request
        { httpRequest =
            AWS.Core.Http.setResponseParser parser req
        , andThen = andThen
        }


requestBodyResult : Http.Response String -> Result String String
requestBodyResult response =
    Ok response.body


stringRequest : Method -> String -> Body -> Request String String
stringRequest method url body =
    parserRequest method url body requestBodyResult identityAndThen


{-| Create a `Request` to list the keys for an S3 bucket.
-}
listKeys : Bucket -> Request String KeyList
listKeys bucket =
    parserRequest GET
        ("/" ++ bucket ++ "/")
        emptyBody
        requestBodyResult
        parseListBucketResponseResult


parseListBucketResponseResult : String -> Task Error KeyList
parseListBucketResponseResult xml =
    case parseListBucketResponse xml of
        Ok res ->
            Task.succeed res

        Err err ->
            Task.fail err


objectPath : Bucket -> Key -> String
objectPath bucket key =
    "/" ++ bucket ++ "/" ++ key


{-| Return a `Request` to read an S3 object.

The contents will be the successful result of the `Task` created by `S3.send`.

-}
getObject : Bucket -> Key -> Request String String
getObject bucket key =
    stringRequest GET (objectPath bucket key) emptyBody


{-| Get an object and process the entire Http Response.

    responseHeaders : Http.Response String -> Result String ( String, List ( String, String ) )
    responseHeaders response =
        Ok <| ( response.body, Dict.toList response.headers )

    getObjectWithHeaders : Bucket -> Key -> Request ( String, List ( String, String ) )
    getObjectWithHeaders bucket key =
        getFullObject bucket key responseHeaders

-}
getFullObject : Bucket -> Key -> (Http.Response String -> Result String a) -> Request a a
getFullObject bucket key parser =
    parserRequest GET
        (objectPath bucket key)
        emptyBody
        parser
        identityAndThen


responseHeaders : Http.Response String -> Result String ( String, List ( String, String ) )
responseHeaders response =
    Ok <| ( response.body, Dict.toList response.headers )


{-| Get an object with its HTTP response headers.
-}
getObjectWithHeaders : Bucket -> Key -> Request ( String, List ( String, String ) ) ( String, List ( String, String ) )
getObjectWithHeaders bucket key =
    getFullObject bucket key responseHeaders


{-| Create an HTML body for `putObject` and friends.
-}
htmlBody : String -> Body
htmlBody =
    AWS.Core.Http.stringBody "text/html;charset=utf-8"


{-| Create a JSON body for `putObject` and friends.
-}
jsonBody : JE.Value -> Body
jsonBody =
    AWS.Core.Http.jsonBody


{-| Create a body with any mimetype for `putObject` and friends.

    stringBody "text/html" "Hello, World!"

-}
stringBody : Mimetype -> String -> Body
stringBody =
    AWS.Core.Http.stringBody


{-| Return a `Request` to write an object to S3, with default permissions (private).

The string resulting from a successful `send` isn't interesting.

-}
putObject : Bucket -> Key -> Body -> Request String String
putObject bucket key body =
    stringRequest PUT
        (objectPath bucket key)
        body


{-| Return a `Request` to write an object to S3, with public-read permission.

The string resulting from a successful `send` isn't interesting.

-}
putPublicObject : Bucket -> Key -> Body -> Request String String
putPublicObject bucket key body =
    putObject bucket key body
        |> addHeaders [ XAmzAcl AclPublicRead ]


{-| Write an Html string to S3, with public-read permission.

The string resulting from a successful `send` isn't interesting.

-}
putHtmlObject : Bucket -> Key -> String -> Request String String
putHtmlObject bucket key html =
    putPublicObject bucket key <| htmlBody html


{-| Return a Request to delete an S3 object.

The string resulting from a successful `send` isn't interesting.

-}
deleteObject : Bucket -> Key -> Request String String
deleteObject bucket key =
    stringRequest DELETE
        (objectPath bucket key)
        emptyBody
