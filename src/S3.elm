module S3 exposing ( S3Account
                   , readS3Accounts, decodeS3Accounts, makeCredentials
                   )

import AWS.Core.Credentials exposing ( Credentials
                                     , fromAccessKeys )

import Http
import Task exposing ( Task )
import Json.Decode as JD exposing ( Decoder )

type alias S3Account =
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

readS3Accounts : Maybe String -> Task Http.Error (Result String (List S3Account))
readS3Accounts maybeUrl =
    let url = case maybeUrl of
                  Just u -> u
                  Nothing -> defaultAccountsUrl
        request = Http.getString url
        getTask = Http.toTask request
    in
        Task.map decodeS3Accounts getTask

makeCredentials : S3Account -> Credentials
makeCredentials account =
    fromAccessKeys account.accessKey account.secretKey

nothingToFalse : Maybe Bool -> Decoder Bool
nothingToFalse value =
    case value of
        Nothing ->
            JD.succeed False
        Just res ->
            JD.succeed res

s3AccountDecoder : Decoder S3Account
s3AccountDecoder =
    JD.map6 S3Account
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

s3AccountsDecoder : Decoder (List S3Account)
s3AccountsDecoder =
    JD.list s3AccountDecoder

decodeS3Accounts : String -> Result String (List S3Account)
decodeS3Accounts json =
    JD.decodeString s3AccountsDecoder json
