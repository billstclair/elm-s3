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

import S3.Types exposing ( Error(..)
                         , StorageClass, Bucket, BucketList
                         )

import Xml exposing ( Value(..) )
import Xml.Decode as XD
import Xml.Query as XQ

parseListBucketResponse : String -> Result Error BucketList
parseListBucketResponse xml =
    -- Need to parse the XML into a list of Buckets.
    case XD.decodeChildren xml of
        Err msg ->
            Err <| MalformedXmlError msg
        Ok value ->
            case value of
                Object vals ->
                    case parseBucketList vals of
                        Ok buckets -> Ok buckets
                        Err msg -> Err <| ParseError msg
                _ ->
                    Err  <| ParseError "Top-level element is not an Object"
                            
getOne : (Value -> Result String a) -> List Value -> String -> Result String a
getOne converter values err =
    case XQ.collect converter values of
        res :: _ ->
            Ok res
        _ ->
            Err err

getZeroOrOne : (Value -> Result String a) -> List Value -> Maybe a
getZeroOrOne converter values =
    case XQ.collect converter values of
        res :: _ ->
            Just res
        _ ->
            Nothing

parseBucketList : List Value -> Result String BucketList
parseBucketList vals =
    case getOne (XQ.tag "ListBucketResult" Ok) vals "Missing ListBucketResult" of
        Err msg ->
            Err msg
        Ok value ->
            case value of
                Object innerVals ->
                    parseInnerBucketList innerVals
                _ ->
                    Err "ListBucketResult is not an Object."

parseInnerBucketList : List Value -> Result String BucketList
parseInnerBucketList vals =
    let prefix = getZeroOrOne (XQ.tag "Prefix" XQ.string) vals
        marker = getZeroOrOne (XQ.tag "Marker" XQ.string) vals
        nextMarker = getZeroOrOne (XQ.tag "NextMarker" XQ.string) vals
        isTruncated = case getZeroOrOne (XQ.tag "IsTruncated" XQ.bool) vals of
                          Just val -> val
                          Nothing -> False
    in
        case getOne (XQ.tag "Name" XQ.string) vals "Missing Name" of
            Err msg -> Err msg
            Ok name ->
                case getOne (XQ.tag "MaxKeys" XQ.int) vals "Missing MaxKeys" of
                    Err msg -> Err msg
                    Ok maxKeys ->
                        case parseBuckets vals of
                            Err msg -> Err msg
                            Ok buckets ->
                                Ok { name = name
                                   , prefix = prefix
                                   , marker = marker
                                   , nextMarker = nextMarker
                                   , maxKeys = maxKeys
                                   , isTruncated = isTruncated
                                   , buckets = buckets
                                   }
                                    
convertEach : (Value -> Result String a) -> (List Value) -> Result String (List a)
convertEach converter values =
    let loop = (\values res ->
                    case values of
                        [] ->
                            Ok <| List.reverse res
                        val :: tail ->
                            case converter val of
                                Err msg ->
                                    Err msg
                                Ok a ->
                                    loop tail (a :: res)
               )
    in
        loop values []

parseBuckets : List Value -> Result String (List Bucket)
parseBuckets values =
    let bucketValues = XQ.collect (XQ.tag "Contents" Ok) values
        count = Debug.log "count" <| List.length bucketValues
    in
        convertEach parseBucket bucketValues

parseBucket : Value -> Result String Bucket
parseBucket value =
    case value of
        Object vals ->
            case ( getOne (XQ.tag "Key" XQ.string) vals "Missing Key"
                 , getOne (XQ.tag "LastModified" XQ.string) vals "Missing LastModified"
                 , getOne (XQ.tag "ETag" XQ.string) vals "Missing ETag"
                 , getOne (XQ.tag "Size" XQ.int) vals "Missing Size"
                 , getOne (XQ.tag "StorageClass" XQ.string) vals "Missing StorageClass"
                 , getOne (XQ.tag "Owner" Ok) vals "Missing Owner"
                 ) of
                ( Ok key, Ok lastModified, Ok eTag, Ok size
                , Ok storageClass, Ok owner) ->
                  case parseOwner owner of
                      Err msg ->
                          Err msg
                      Ok id ->
                          Ok { key = key
                             , lastModified = lastModified
                             , eTag = eTag
                             , size = size
                             , storageClass = storageClass
                             , owner = id
                             }
                _ ->
                    Err "Bad Bucket element."
        _ ->
            Err "Bucket XML not an Object."

parseOwner : Value -> Result String String
parseOwner value =
    case value of
        Object vals ->
            case getOne (XQ.tag "DisplayName" XQ.int) vals "Missing ID" of
                Ok res ->
                    Ok <| toString res
                Err _ ->
                    getOne (XQ.tag "DisplayName" XQ.string) vals "Missing ID"
        _ ->
            Err "Owner is not an Object."
