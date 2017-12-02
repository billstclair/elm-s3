----------------------------------------------------------------------
--
-- S3/Parser.elm
-- XML parser for elm-s3.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module S3.Parser exposing ( parseListBucketResponse
                          )

import S3.Types exposing ( Error ( MalformedXmlError, ParseError )
                         , StorageClass, Owner, KeyInfo, KeyList
                         )

import Xml.Extra exposing ( TagSpec, Required ( Required, Optional, Multiple )
                          , decodeXml, requiredTag, optionalTag, multipleTag
                          )

import Json.Decode as JD exposing ( Decoder )

makeError : Xml.Extra.Error -> Error
makeError error =
    case error of
        Xml.Extra.XmlError msg ->
            MalformedXmlError msg
        Xml.Extra.DecodeError details ->
            ParseError details

parseListBucketResponse : String -> Result Error KeyList
parseListBucketResponse xml =
    case decodeXml xml "ListBucketResult" listBucketDecoder listBucketTagSpecs of
        Err err ->
            Err <| makeError err
        Ok res ->
            Ok res

intOrString : Decoder String
intOrString =
    JD.oneOf
        [ JD.string
        , JD.int |> JD.andThen (\int -> JD.succeed (toString int))
        ]

ownerDecoder : Decoder Owner
ownerDecoder =
    JD.map2 Owner
        (JD.field "ID" intOrString)
        (JD.field "DisplayName" intOrString)

ownerTagSpecs : List TagSpec
ownerTagSpecs =
    [ ("ID", Required)
    , ("DisplayName", Required)
    ]

defaultOwner : Owner
defaultOwner =
    { id = "nothing"
    , displayName = "nobody"
    }

bucketDecoder : Decoder KeyInfo
bucketDecoder =
    JD.map6 KeyInfo
        (JD.field "Key" JD.string)
        (JD.field "LastModified" JD.string)
        (JD.field "ETag" JD.string)
        (JD.field "Size" JD.int)
        (JD.field "StorageClass" JD.string)
        (requiredTag "Owner" ownerDecoder ownerTagSpecs)

-- DigitalOcean puts Owner after StorageClass
bucketTagSpecs : List TagSpec
bucketTagSpecs =
    [ ("Key", Required)
    , ("LastModified", Required)
    , ("ETag", Required)
    , ("Size", Required)
    , ("StorageClass", Required)
    , ("Owner", Required)
    ]

-- Amazon S3 puts Owner before StorageClass
amazonBucketTagSpecs : List TagSpec
amazonBucketTagSpecs =
    [ ("Key", Required)
    , ("LastModified", Required)
    , ("ETag", Required)
    , ("Size", Required)
    , ("Owner", Required)
    , ("StorageClass", Required)
    ]

boolOrString : Decoder Bool
boolOrString =
    JD.oneOf
        [ JD.bool
        , JD.string
            |> JD.andThen
               (\s -> JD.succeed <| s == "true")
        ]                    

listBucketDecoder : Decoder KeyList
listBucketDecoder =
    JD.map7 KeyList
        (JD.field "Name" JD.string)
        (optionalTag "Prefix" JD.string [])
        (optionalTag "Marker" JD.string [])
        (optionalTag "NextMarker" JD.string [])
        (JD.field "MaxKeys" JD.int)
        (requiredTag "IsTruncated" boolOrString [])
        (JD.oneOf
             [ multipleTag "Contents" bucketDecoder bucketTagSpecs
             , multipleTag "Contents" bucketDecoder amazonBucketTagSpecs
             ]
        )

listBucketTagSpecs : List TagSpec
listBucketTagSpecs =
    [ ("Name", Required)
    , ("Prefix", Optional)
    , ("Marker", Optional)
    , ("NextMarker", Optional)
    , ("MaxKeys", Required)
    , ("IsTruncated", Required)
    , ("Contents", Multiple)
    ]
