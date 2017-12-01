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

module S3 exposing ( readAccounts, decodeAccounts
                   , htmlBody, jsonBody, stringBody
                   , listKeys, getObject, deleteObject
                   , putObjectWithHeaders, putObject, putHtmlObject
                   )

{-| Elm client for the AWS Simple Storage Service (S3) or Digital Ocean Spaces.

# Functions

@docs readAccounts, decodeAccounts
@docs htmlBody, jsonBody, stringBody
@docs listKeys, getObject, deleteObject
@docs putObjectWithHeaders, putObject, putHtmlObject

-}

import S3.Types exposing ( Error(..), Account
                         , StorageClass, Key, KeyList
                         , Query, QueryElement(..)
                         , CannedAcl(..), aclToString
                         )

import S3.Parser exposing ( parseListBucketResponse
                          )

import AWS.Core.Service as Service exposing ( Service, ApiVersion, Protocol )
import AWS.Core.Credentials exposing ( Credentials
                                     , fromAccessKeys )
import AWS.Core.Http exposing ( Method(..), Request, Body
                              , emptyBody
                              , request, addQuery, addHeaders
                              )

import Http
import Task exposing ( Task )
import Json.Decode as JD exposing ( Decoder )
import Json.Encode as JE

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
    let url = case maybeUrl of
                  Just u -> u
                  Nothing -> defaultAccountsUrl
        request = Http.getString url
        getTask = Http.toTask request
    in
        Task.andThen decodeAccountsTask
            <| Task.onError handleHttpError getTask

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

serviceGetters : Bool -> Service -> Service
serviceGetters isDigitalOcean =
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
        (JD.map serviceGetters
             <| JD.oneOf
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
makeService { region, serviceModifier } =
    case region of
        Nothing ->
            Service.defineGlobal
                endpointPrefix apiVersion protocol Service.signV4 serviceModifier
        Just region ->
            Service.defineRegional
                endpointPrefix apiVersion protocol Service.signV4 serviceModifier region

send : Account -> Request a -> Task Http.Error a
send account req =
    let service = makeService account
        credentials = makeCredentials account
    in
        AWS.Core.Http.send service credentials req

formatQuery : Query -> List (String, String)
formatQuery query =
    let formatElement = (\element ->
                             case element of
                                 Delimiter s -> ("delimiter", s)
                                 Marker s -> ("marker", s)
                                 MaxKeys cnt -> ("max-keys", toString cnt)
                                 Prefix s -> ("prefix", s)
                                 XAmzAcl acl -> ("x-amz-acl", aclToString acl)
                        )
    in
        List.map formatElement query

{-| List the keys for an S3 bucket.

The Query is used to specify the delimiter, marker, max-keys, and prefix for the request.
-}
listKeys : Account -> String -> Query -> Task Error KeyList
listKeys account bucket query =
    let req = request GET
                  ("/" ++ bucket ++ "/")
                  emptyBody
                  JD.string
                  |> addQuery (formatQuery query)
        task = send account req
    in
        Task.andThen parseListBucketResponseTask
            <| Task.onError handleBadPayload task

parseListBucketResponseTask : String -> Task Error KeyList
parseListBucketResponseTask xml =
    case parseListBucketResponse xml of
        Err err ->
            Task.fail err
        Ok buckets ->
            Task.succeed buckets

handleBadPayload : Http.Error -> Task Error String
handleBadPayload error =
    case error of
        Http.BadPayload _ response ->
            Task.succeed response.body
        _ ->
            Task.fail <| HttpError error

objectPath : String -> String -> String
objectPath bucket key =
    "/" ++ bucket ++ "/" ++ key

{-| Read an S3 object.

The two `String` parameters are bucket and key.
-}
getObject : Account -> String -> String -> Task Error String
getObject account bucket key =
    let req = request GET (objectPath bucket key) emptyBody JD.string
    in
        send account req
            |> Task.onError handleBadPayload

{-| Create an HTML body for `putObject` and `putObjectWithHeaders`.
-}
htmlBody : String -> Body
htmlBody =
    AWS.Core.Http.stringBody "text/html"

{-| Create a JSON body for `putObject` and `putObjectWithHeaders`.
-}
jsonBody : JE.Value -> Body
jsonBody =
    AWS.Core.Http.jsonBody

{-| Create an body with any mimetype for `putObject` and `putObjectWithHeaders`.

    stringBody mimetype string

Where `mimetype` is, for example, `"text/html"`.
-}
stringBody : String -> String -> Body
stringBody =
    AWS.Core.Http.stringBody

{-| Write an object to S3, with headers that can control, for example, the ACL.

The two `String` parameters are bucket and key.
-}
putObjectWithHeaders : Account -> String -> String -> Query -> Body -> Task Error String
putObjectWithHeaders account bucket key headers body =
    let req = request PUT
              (objectPath bucket key)
              body
              JD.string
              |> addHeaders (formatQuery headers)
    in
        send account req
            |> Task.onError handleBadPayload

{-| Write an object to S3, with public-read permission.

The two `String` parameters are bucket and key.
-}
putObject : Account -> String -> String -> Body -> Task Error String
putObject account bucket key body =
    putObjectWithHeaders account bucket key [XAmzAcl AclPublicRead] body

{-| Write an Html string to S3, with public-read permission.

The first two `String` parameters are bucket and key.

The third `String` parameter is the string to write to the object.
-}
putHtmlObject : Account -> String -> String -> String -> Task Error String
putHtmlObject account bucket key html =
    putObject account bucket key <| htmlBody html

{-| Delete an S3 object.

The two `String` parameters are bucket and key.
-}
deleteObject : Account -> String -> String -> Task Error String
deleteObject account bucket key =
    let req = request DELETE
              (objectPath bucket key)
              emptyBody
              JD.string
    in
        send account req
            |> Task.onError handleBadPayload
