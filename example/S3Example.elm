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
                     )
import Html.Attributes exposing ( href, type_, size, value, disabled, style )
import Html.Events exposing ( onClick, onInput )
import Http
import Json.Decode as JD
import Task
import Char
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
    , bucket : String
    }

type Msg
    = SetBucket String
    | ListBucket
    | ReceiveListBucket (Result Http.Error String)
    | ReceiveAccounts (Result Http.Error (Result String (List S3Account)))

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

makeService : String -> Service
makeService region =
    let s3 = Service.defineRegional endpointPrefix apiVersion protocol Service.signV4
    in
        s3 (Service.setIsDigitalOcean True) region

init : (Model, Cmd Msg)
init =
    ( { accounts = []
      , service = makeService digitalOceanRegion
      , display = "Fetching accounts..."
      , bucket = "etwof"
      }
    , Task.attempt ReceiveAccounts (readS3Accounts Nothing)
    )

listBucket : Model -> Cmd Msg
listBucket model =
    case model.accounts of
        account :: _ ->
            let req = request GET ("/" ++ model.bucket ++ "/") [] emptyBody JD.string
                host = Service.host model.service
                credentials = makeCredentials account
                task = send model.service credentials req
            in
                Task.attempt ReceiveListBucket task
        _ ->
            Cmd.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
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
            case result of
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
                            ( { model | accounts = accounts
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
        , p [] [ input [ type_ "text"
                       , value model.bucket
                       , onInput SetBucket
                       ]
                     []
               , text " "
               , button [ onClick ListBucket ]
                   [ text "List Bucket" ]
               ]
        ]
