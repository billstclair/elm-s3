----------------------------------------------------------------------
--
-- S3Example.elm
-- Example of using the S3 library
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module S3Example exposing (..)

import S3 exposing ( readAccounts, makeCredentials )
import S3.Types exposing ( Error(..), Account, Bucket, BucketList )

import AWS.Core.Service as Service

import Html exposing ( Html, Attribute
                     , div, text, p, h2, table, tr, th, td, a, br
                     , input, button
                     , select, option
                     )
import Html.Attributes exposing ( href, type_, size, value, disabled, style
                                , selected
                                )
import Html.Events exposing ( onClick, onInput, on, targetValue )
import Http
import Json.Decode as JD
import Task
import Char
import List.Extra as LE
import Debug exposing ( log )

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }

type alias Model =
    { display : String
    , accounts : List Account
    , account : Account
    , bucket : String
    , key : String
    }

type Msg
    = SetAccount String
    | ReceiveAccounts (Result Error (List Account))
    | ReceiveGetObject (Result Error String)
    | SetBucket String
    | ListBucket
    | ReceiveListBucket (Result Error BucketList)
    | SetKey String
    | GetObject

init : (Model, Cmd Msg)
init =
    ( { display = "Fetching accounts..."
      , accounts = []
      , account = defaultAccount
      , bucket = "No bucket"
      , key = ""
      }
    , Task.attempt ReceiveAccounts (readAccounts Nothing)
    )

listBucket : Model -> Cmd Msg
listBucket model =
    let task = S3.listBucket model.account model.bucket
    in
        Task.attempt ReceiveListBucket task

getObject : Model -> Cmd Msg
getObject model =
    let task = S3.getObject model.account model.bucket model.key
    in
        Task.attempt ReceiveGetObject task

defaultAccount : Account
defaultAccount =
    { name = "No account"
    , region = Nothing
    , accessKey = ""
    , secretKey = ""
    , buckets = [ "No bucket" ]
    , serviceGetters = Service.s3Getters
    }

findAccount : Model -> String -> Account
findAccount model name =
    case LE.find (\a -> a.name == name) model.accounts of
        Nothing ->
            defaultAccount
        Just a ->
            a

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetAccount name ->
            let account = findAccount model name
                bucket = case account.buckets of
                             b :: _ ->
                                 b
                             _ ->
                                 "No bucket"
            in
                ( { model
                      | account = account
                      , bucket = bucket
                      , display = "Account: " ++ name
                  }
                , Cmd.none
            )
        SetBucket bucket ->
            ( { model | bucket = bucket }
            , Cmd.none
            )
        ListBucket ->
            ( model
            , listBucket model
            )
        ReceiveListBucket result ->
            case result of
                Err err ->
                    ( { model | display = toString err }
                    , Cmd.none
                    )
                Ok buckets ->
                    ( { model | display = toString buckets }
                    , Cmd.none
                    )
        SetKey key ->
            ( { model | key = key }
            , Cmd.none
            )
        GetObject ->
            if model.key == "" then
                ( { model | display = "Blank key." }
                , Cmd.none
                )
            else
                ( { model | display = "Fetching " ++ model.key ++ "..." }
                , getObject model
                )
        ReceiveGetObject result ->
            case result of
                Err err ->
                    ( { model | display = toString err }
                    , Cmd.none
                    )
                Ok res ->
                    -- This won't happen, since the JSON string decoder will fail
                    ( { model | display = res }
                    , Cmd.none
                    )
        ReceiveAccounts result ->
            case result of
                Err err ->
                    ( { model | display = toString err }
                    , Cmd.none
                    )
                Ok accounts ->
                    let account = case accounts of
                                      a :: _ ->
                                          a
                                      _ ->
                                          defaultAccount
                    in
                        ( { model
                              | accounts = accounts
                              , account = account
                              , bucket = case account.buckets of
                                             b :: _ ->
                                                 b
                                             _ ->
                                                 "No bucket"
                              , display = "Accounts received."
                          }
                        , Cmd.none
                        )
                        
processReceiveBucket : String -> Model -> (Model, Cmd Msg)
processReceiveBucket xml model =
    ( { model | display = xml }
      , Cmd.none
    )

view : Model -> Html Msg
view model =
    div []
        [ p [] [ text model.display ]
        , p [] [ text "Account: "
               , accountSelector model
               ]
        , p [] [ text "Bucket: "
               , bucketSelector model
               , text " "
               , button [ onClick ListBucket ]
                   [ text "List Bucket" ]
               ]
        , p [] [ text "Key: "
               , input [ type_ "text"
                       , size 40
                       , value model.key
                       , onInput SetKey
                       ]
                     []
               , text " "
               , button [ onClick GetObject ]
                   [ text "Get Object" ]
               ]
            ]

accountSelector : Model -> Html Msg
accountSelector model =
    select [ on "change" (JD.map SetAccount targetValue) ]
        (List.map (accountOption model) model.accounts)

accountOption : Model -> Account -> Html Msg
accountOption model account =
    option [ value account.name
           , selected (model.account.name == account.name)
           ]
        [ text account.name ]

bucketSelector : Model -> Html Msg
bucketSelector model =
    select [ on "change" (JD.map SetBucket targetValue) ]
        (List.map (bucketOption model) model.account.buckets)

bucketOption : Model -> String -> Html Msg
bucketOption model bucket =
    option [ value bucket
           , selected (model.bucket == bucket)
           ]
        [ text bucket ]
