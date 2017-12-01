----------------------------------------------------------------------
--
-- S3/Types.elm
-- Types for elm-s3
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module S3.Types exposing ( Error(..), Account
                         , StorageClass, Owner, Key, KeyList
                         , Query, QueryElement(..)
                         , CannedAcl(..), aclToString
                         )

{-| Types for S3 module

# Types

@docs Error, Account, StorageClass, Owner, Key, KeyList, Query, QueryElement, CannedAcl

# Functions

@docs aclToString
-}

import Xml.Extra exposing ( DecodeDetails )

import AWS.Core.Service as Service

import Http

{-| Errors returned from S3 operations

`Http.Error` is from the standard Elm `Http` module.

`MalformedXmlError` denotes an error in parsing the raw XML returned by S3.

`ParseError` denotes an error turning the parsed XML into an Elm object.

`DecodeError` denotes a Decoder error in parsing S3 account info.
-}
type Error
    = HttpError Http.Error
    | MalformedXmlError String
    | ParseError DecodeDetails
    | DecodeError String

{-| Information about en S3 account
-}
type alias Account =
    { name : String
    , region : Maybe String
    , accessKey : String
    , secretKey : String
    , buckets : List String
    , serviceGetters : Service.Getters
    }

{-| The StorageClass for a key returned from listing a bucket's contents.
-}
type alias StorageClass =
    String

{-| The owner of an object returned from listing a bucket's contents.
-}
type alias Owner =
    { id : String
    , displayName : String
    }
          
{-| Information about a single key returned from listing a bucket's contents.
-}
type alias Key =
    { key : String
    , lastModified : String
    , eTag : String
    , size: Int
    , storageClass : StorageClass
    , owner : Owner
    }

{-| All the information returned from listing a bucket's contents.

An Elm encoding of the ListBucketResult XML element.
-}
type alias KeyList =
    { name : String
    , prefix : Maybe String
    , marker : Maybe String
    , nextMarker : Maybe String
    , maxKeys : Int
    , isTruncated : Bool
    , keys : List Key
    }

{-| Values for the XAmzAcl Query type.
-} 
-- http://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl
type CannedAcl
    = AclPrivate
    | AclPublicRead
    | AclPublicReadWrite
    | AclAwsExecRead
    | AclAuthenticatedRead
    | AclBucketOwnerRead
    | AclBucketOwnerFullControl
    | AclLogDeliveryWrite

{-| Convert a `CannedAcl` to a String.
-}
aclToString : CannedAcl -> String
aclToString acl =
    case acl of
        AclPrivate ->
            "private"
        AclPublicRead ->
            "public-read"
        AclPublicReadWrite ->
            "public-read-write"
        AclAwsExecRead ->
            "aws-exec-read"
        AclAuthenticatedRead ->
            "authenticated-read"
        AclBucketOwnerRead ->
            "bucket-owner-read"
        AclBucketOwnerFullControl ->
            "bucket-owner-full-control"
        AclLogDeliveryWrite ->
            "log-delivery-write"

{-| An element of a `Query`, used for HTTP headers and query parameters.

`XAmzAcl` is used as a header with `S3.putObject`.

The others are used as query parameters with `S3.listKeys`.
-}
type QueryElement
    = Delimiter String
    | Marker String
    | MaxKeys Int
    | Prefix String
    | XAmzAcl CannedAcl

{-| A list of `QueryElement`s.
-}
type alias Query =
    List QueryElement
