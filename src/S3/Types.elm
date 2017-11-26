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
                         , StorageClass, Bucket, BucketList
                         )

import AWS.Core.Service as Service

import Http

type Error
    = HttpError Http.Error
    | MalformedXmlError String
    | ParseError String

type alias Account =
    { name : String
    , region : Maybe String
    , accessKey : String
    , secretKey : String
    , buckets : List String
    , serviceGetters : Service.Getters
    }

type alias StorageClass =
    String

type alias Bucket =
    { key : String
    , lastModified : String
    , eTag : String
    , size: Int
    , storageClass : StorageClass
    , owner : String
    }

type alias BucketList =
    { name : String
    , prefix : Maybe String
    , marker : Maybe String
    , nextMarker : Maybe String
    , maxKeys : Int
    , isTruncated : Bool
    , buckets : List Bucket
    }


