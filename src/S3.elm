module S3 exposing ( Account, StorageClass(..), ObjectOwner, Bucket
                   , readAccounts, decodeAccounts, makeCredentials
                   , listBucket, getObject
                   )

import AWS.Core.Service as Service exposing ( Service, ApiVersion, Protocol )
import AWS.Core.Credentials exposing ( Credentials
                                     , fromAccessKeys )
import AWS.Core.Http exposing ( Method(..), Response
                              , responseData, emptyBody
                              , request, send
                              )

import Http
import Task exposing ( Task )
import Json.Decode as JD exposing ( Decoder )

type alias Account =
    { name : String
    , region : Maybe String
    , isDigitalOcean : Bool
    , accessKey : String
    , secretKey : String
    , buckets : List String
    }

defaultAccountsUrl : String
defaultAccountsUrl =
    "accounts.json"

readAccounts : Maybe String -> Task Http.Error (Result String (List Account))
readAccounts maybeUrl =
    let url = case maybeUrl of
                  Just u -> u
                  Nothing -> defaultAccountsUrl
        request = Http.getString url
        getTask = Http.toTask request
    in
        Task.map decodeAccounts getTask

makeCredentials : Account -> Credentials
makeCredentials account =
    fromAccessKeys account.accessKey account.secretKey

accountDecoder : Decoder Account
accountDecoder =
    JD.map6 Account
        (JD.field "name" JD.string)
        (JD.oneOf
             [ JD.field "region" (JD.nullable JD.string)
             , JD.succeed Nothing
             ]
        )
        (JD.oneOf
             [ JD.field "is-digital-ocean" JD.bool
             , JD.succeed False
             ]
        )
        (JD.field "access-key" JD.string)
        (JD.field "secret-key" JD.string)
        (JD.field "buckets" (JD.list JD.string))

accountsDecoder : Decoder (List Account)
accountsDecoder =
    JD.list accountDecoder

decodeAccounts : String -> Result String (List Account)
decodeAccounts json =
    JD.decodeString accountsDecoder json

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
    let sdo = Service.setIsDigitalOcean account.isDigitalOcean
    in
        case account.region of
            Nothing ->
                Service.defineGlobal
                    endpointPrefix apiVersion protocol Service.signV4 sdo
            Just region ->
                Service.defineRegional
                    endpointPrefix apiVersion protocol Service.signV4 sdo region

type StorageClass
    = StandardClass

type alias ObjectOwner =
    { id : String
    , displayName : String
    }

type alias Bucket =
    { key : String
    , lastModified : String
    , eTag : String
    , size: Int
    , storageClass : StorageClass
    , owner : ObjectOwner
    }

listBucket : Account -> String -> Task Http.Error (Result String (List Bucket))
listBucket account bucket =
    let service = makeService account
        req = request GET ("/" ++ bucket ++ "/") [] emptyBody JD.string
        credentials = makeCredentials account
        task = send service credentials req
    in
        Task.map parseListBucketResponse
            <| Task.onError handleBadPayload task

handleBadPayload : Http.Error -> Task Http.Error String
handleBadPayload error =
    case error of
        Http.BadPayload _ response ->
            Task.succeed response.body
        _ ->
            Task.fail error

parseListBucketResponse : String -> Result String (List Bucket)
parseListBucketResponse xml =
    Err xml

getObject : Account -> String -> String -> Task Http.Error String
getObject account bucket key =
    let service = makeService account
        req = request GET ("/" ++ bucket ++ "/" ++ key)
              [] emptyBody JD.string
        credentials = makeCredentials account
    in
        send service credentials req
    
