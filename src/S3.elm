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

module S3 exposing ( readAccounts, decodeAccounts, makeCredentials
                   , listBucket, getObject
                   )

import S3.Types exposing ( Error(..), Account
                         , StorageClass, Bucket, BucketList
                         )

import S3.Parser exposing ( parseListBucketResponse
                          )

import AWS.Core.Service as Service exposing ( Service, ApiVersion, Protocol )
import AWS.Core.Credentials exposing ( Credentials
                                     , fromAccessKeys )
import AWS.Core.Http exposing ( Method(..), Request, Response
                              , responseData, emptyBody
                              , request
                              )

import Http
import Task exposing ( Task )
import Json.Decode as JD exposing ( Decoder )

defaultAccountsUrl : String
defaultAccountsUrl =
    "accounts.json"

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

serviceGetters : Bool -> Service.Getters
serviceGetters isDigitalOcean =
    if isDigitalOcean then
        Service.digitalOceanGetters
    else
        Service.s3Getters

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

decodeAccounts : String -> Result Error (List Account)
decodeAccounts json =
    case JD.decodeString accountsDecoder json of
        Err s ->
            Err <| ParseError s
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
makeService account =
    let sdo = Service.setGetters account.serviceGetters
    in
        case account.region of
            Nothing ->
                Service.defineGlobal
                    endpointPrefix apiVersion protocol Service.signV4 sdo
            Just region ->
                Service.defineRegional
                    endpointPrefix apiVersion protocol Service.signV4 sdo region

send : Account -> Request a -> Task Http.Error a
send account req =
    let service = makeService account
        credentials = makeCredentials account
    in
        AWS.Core.Http.send service credentials req

listBucket : Account -> String -> Task Error BucketList
listBucket account bucket =
    let req = request GET ("/" ++ bucket ++ "/") [] emptyBody JD.string
        task = send account req
    in
        Task.andThen parseListBucketResponseTask
            <| Task.onError handleBadPayload task

parseListBucketResponseTask : String -> Task Error BucketList
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

getObject : Account -> String -> String -> Task Error String
getObject account bucket key =
    let req = request GET ("/" ++ bucket ++ "/" ++ key)
              [] emptyBody JD.string        
    in
        send account req
            |> Task.onError handleBadPayload
