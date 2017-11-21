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

import S3 exposing ( S3Account, readS3Accounts, makeCredentials )

import AWS.Core.Credentials exposing ( Credentials )
import AWS.Core.Service as Service exposing ( Service, ApiVersion, Protocol )
import AWS.Core.Http exposing ( Method(..), Response
                              , responseData, emptyBody
                              , request, send
                              )

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
    { accounts : List S3Account
    , service : Service
    , display : String
    , account : S3Account
    , bucket : String
    , path : String
    }

type Msg
    = SetAccount String
    | SetBucket String
    | ListBucket
    | ReceiveListBucket (Result Http.Error String)
    | ReceiveAccounts (Result Http.Error (Result String (List S3Account)))
    | SetPath String
    | ShowPath

endpointPrefix : String
endpointPrefix =
    "s3"

apiVersion : ApiVersion
apiVersion =
    "2017-07-10"

protocol : Protocol
protocol =
    Service.restXml

digitalOceanRegion : String
digitalOceanRegion =
    "nyc3"

makeInitialService : String -> Service
makeInitialService region =
    let s3 = Service.defineRegional endpointPrefix apiVersion protocol Service.signV4
    in
        s3 (Service.setIsDigitalOcean True) region

makeService : S3Account -> Service
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

init : (Model, Cmd Msg)
init =
    ( { accounts = []
      , service = makeInitialService digitalOceanRegion
      , account = defaultAccount
      , display = "Fetching accounts..."
      , bucket = "No bucket"
      , path = ""
      }
    , Task.attempt ReceiveAccounts (readS3Accounts Nothing)
    )

listBucket : Model -> Cmd Msg
listBucket model =
    let req = request GET ("/" ++ model.bucket ++ "/") [] emptyBody JD.string
        credentials = makeCredentials model.account
        task = send model.service credentials req
    in
        Task.attempt ReceiveListBucket task

defaultAccount : S3Account
defaultAccount =
    { name = "No account"
    , region = Nothing
    , isDigitalOcean = False
    , accessKey = ""
    , secretKey = ""
    , buckets = [ "No bucket" ]
    }

findAccount : Model -> String -> S3Account
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
                      , service = makeService account
                      , bucket = bucket
                      , display = "Account: " ++ (toString account)
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
                    case err of
                        Http.BadPayload _ response ->
                            processReceiveBucket response.body model
                        _ ->
                            ( { model | display = toString err }
                            , Cmd.none
                            )
                Ok res ->
                    -- This won't happen, since the JSON string decoder will fail
                    ( { model | display = res }
                    , Cmd.none
                    )
        ReceiveAccounts result ->
            case log "accounts" result of
                Err err ->
                    ( { model | display = toString err }
                    , Cmd.none
                    )
                Ok decoded ->
                    case decoded of
                        Err msg ->
                            ( { model | display = msg }
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
                                    , service = makeService account
                                    , bucket = case account.buckets of
                                                   b :: _ ->
                                                       b
                                                   _ ->
                                                       "No bucket"
                                    , display = "Accounts received."
                                }
                              , Cmd.none
                              )
        SetPath path ->
            ( { model | path = path }
            , Cmd.none
            )
        ShowPath ->
            ( { model | display = "ShowPath not yet implemented." }
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
               , button [ onClick ListBucket ]
                   [ text "List Bucket" ]
               ]
        , p [] [ text "Path: "
               , input [ type_ "text"
                       , size 40
                       , value model.path
                       , onInput SetPath
                       ]
                     []
               , button [ onClick ShowPath ]
                   [ text "Show Path" ]
               ]
            ]

accountSelector : Model -> Html Msg
accountSelector model =
    select [ on "change" (JD.map SetAccount targetValue) ]
        (List.map (accountOption model) model.accounts)

accountOption : Model -> S3Account -> Html Msg
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
